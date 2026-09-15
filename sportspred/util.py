"""Small shared helpers: parsing, math, dates, JSON/CSV IO, HTTP with cache."""
from __future__ import annotations

import csv
import json
import math
import os
import random
import ssl
import time
import urllib.error
import urllib.request
from datetime import date, datetime, timedelta, timezone

BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


# ── numbers ──────────────────────────────────────────────────────────────────
def num(value, default=None):
    """Parse anything to float, tolerating '', 'NA', None, '12.3%'."""
    if value is None:
        return default
    if isinstance(value, (int, float)):
        return default if isinstance(value, float) and math.isnan(value) else float(value)
    s = str(value).strip().replace('%', '').replace(',', '')
    if s == '' or s.upper() in ('NA', 'N/A', 'NAN', 'NULL', '-', '--', 'TBD'):
        return default
    try:
        return float(s)
    except ValueError:
        return default


def clamp(x, lo, hi):
    return lo if x < lo else (hi if x > hi else x)


def logistic(x):
    if x >= 0:
        return 1.0 / (1.0 + math.exp(-min(x, 700.0)))
    e = math.exp(max(x, -700.0))
    return e / (1.0 + e)


def logit(p, eps=1e-6):
    p = clamp(p, eps, 1 - eps)
    return math.log(p / (1 - p))


def mean(xs, default=0.0):
    xs = [x for x in xs if x is not None]
    return sum(xs) / len(xs) if xs else default


def stdev(xs, default=0.0):
    xs = [x for x in xs if x is not None]
    if len(xs) < 2:
        return default
    m = sum(xs) / len(xs)
    return math.sqrt(sum((x - m) ** 2 for x in xs) / (len(xs) - 1))


# ── scoring rules ────────────────────────────────────────────────────────────
def brier(prob, outcome):
    return (prob - outcome) ** 2


def log_loss(prob, outcome, eps=1e-9):
    p = clamp(prob, eps, 1 - eps)
    return -(outcome * math.log(p) + (1 - outcome) * math.log(1 - p))


# ── distributions (pure python, no scipy) ────────────────────────────────────
def poisson_pmf(k, lam):
    if lam <= 0:
        return 1.0 if k == 0 else 0.0
    if k < 0:
        return 0.0
    return math.exp(-lam + k * math.log(lam) - math.lgamma(k + 1))


def poisson_cdf(k, lam):
    """P(X <= k)."""
    if k < 0:
        return 0.0
    total, term = 0.0, None
    for i in range(0, int(k) + 1):
        term = poisson_pmf(i, lam)
        total += term
        if total >= 1.0:
            return 1.0
    return clamp(total, 0.0, 1.0)


def poisson_sf(k, lam):
    """P(X > k)."""
    return clamp(1.0 - poisson_cdf(k, lam), 0.0, 1.0)


def negbin_sf(k, mu, var):
    """P(X > k) for a negative binomial with the given mean and variance.

    Falls back to Poisson when the requested variance is not over-dispersed.
    """
    if var <= mu * 1.001 or mu <= 0:
        return poisson_sf(k, mu)
    r = mu * mu / (var - mu)
    p = r / (r + mu)          # P(success); X counts failures
    total = 0.0
    log_pr = r * math.log(p)
    for i in range(0, int(k) + 1):
        lp = log_pr + math.lgamma(i + r) - math.lgamma(r) - math.lgamma(i + 1) + i * math.log(1 - p)
        total += math.exp(lp)
        if total >= 1.0:
            return 0.0
    return clamp(1.0 - total, 0.0, 1.0)


def norm_cdf(x, mu=0.0, sigma=1.0):
    if sigma <= 0:
        return 1.0 if x >= mu else 0.0
    return 0.5 * (1.0 + math.erf((x - mu) / (sigma * math.sqrt(2.0))))


def norm_sf(x, mu=0.0, sigma=1.0):
    return clamp(1.0 - norm_cdf(x, mu, sigma), 0.0, 1.0)


# ── dates ────────────────────────────────────────────────────────────────────
def parse_date(s):
    if not s:
        return None
    try:
        return datetime.strptime(str(s)[:10], '%Y-%m-%d').date()
    except ValueError:
        return None


def days_between(a, b):
    if not a or not b:
        return None
    return (a - b).days


def today_utc():
    return datetime.now(timezone.utc).date()


def now_iso():
    return datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ')


def _nth_weekday(year, month, weekday, n):
    """Date of the n-th given weekday of a month (Monday == 0)."""
    d = date(year, month, 1)
    shift = (weekday - d.weekday()) % 7
    return d + timedelta(days=shift + 7 * (n - 1))


def eastern_offset(dt_utc):
    """Hours to add to UTC for US Eastern time on this instant.

    Computed from the DST rule rather than read from a timezone database, so
    the build does not depend on tzdata being present in the runner image.
    DST runs from 2am local on the second Sunday in March to 2am local on the
    first Sunday in November.
    """
    y = dt_utc.year
    start = datetime.combine(_nth_weekday(y, 3, 6, 2), datetime.min.time(),
                             tzinfo=timezone.utc) + timedelta(hours=7)   # 2am EST
    end = datetime.combine(_nth_weekday(y, 11, 6, 1), datetime.min.time(),
                           tzinfo=timezone.utc) + timedelta(hours=6)     # 2am EDT
    return -4 if start <= dt_utc < end else -5


def format_eastern(iso_or_time, game_date=None):
    """Render a kickoff time as US Eastern.

    Accepts a full ISO timestamp, or the legacy ``HH:MM AM ET`` string the R
    ingest used to emit — which was actually UTC wearing an ET label, so it is
    reinterpreted here rather than displayed as-is.
    """
    if not iso_or_time:
        return ''
    text = str(iso_or_time).strip()
    dt = None
    for fmt in ('%Y-%m-%dT%H:%M:%SZ', '%Y-%m-%dT%H:%MZ', '%Y-%m-%d %H:%M:%S'):
        try:
            dt = datetime.strptime(text, fmt).replace(tzinfo=timezone.utc)
            break
        except ValueError:
            continue
    if dt is None:
        cleaned = text.replace(' ET', '').replace('ET', '').strip()
        base = parse_date(game_date) or today_utc()
        for fmt in ('%I:%M %p', '%H:%M'):
            try:
                t = datetime.strptime(cleaned, fmt).time()
                dt = datetime.combine(base, t, tzinfo=timezone.utc)
                break
            except ValueError:
                continue
    if dt is None:
        return text
    local = dt + timedelta(hours=eastern_offset(dt))
    return local.strftime('%-I:%M %p ET')


# ── file IO ──────────────────────────────────────────────────────────────────
def read_csv(path):
    if not os.path.exists(path) or os.path.getsize(path) < 5:
        return []
    with open(path, newline='', encoding='utf-8') as f:
        return [dict(r) for r in csv.DictReader(f)]


def write_csv(path, rows, fieldnames=None):
    if not rows:
        return
    if fieldnames is None:
        fieldnames = list(rows[0].keys())
        for r in rows:
            for k in r:
                if k not in fieldnames:
                    fieldnames.append(k)
    os.makedirs(os.path.dirname(os.path.abspath(path)) or '.', exist_ok=True)
    with open(path, 'w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=fieldnames, quoting=csv.QUOTE_MINIMAL,
                           extrasaction='ignore')
        w.writeheader()
        for r in rows:
            w.writerow(r)


def read_json(path, default=None):
    try:
        with open(path, encoding='utf-8') as f:
            return json.load(f)
    except (OSError, ValueError):
        return default if default is not None else {}


def write_json(path, obj, indent=1):
    """Write JSON atomically. ``indent=None`` gives the compact form, which
    matters for files this repository rewrites every hour."""
    os.makedirs(os.path.dirname(os.path.abspath(path)) or '.', exist_ok=True)
    tmp = path + '.tmp'
    separators = (',', ':') if indent is None else None
    with open(tmp, 'w', encoding='utf-8') as f:
        json.dump(obj, f, indent=indent, separators=separators,
                  sort_keys=True, default=str)
    os.replace(tmp, path)


# ── HTTP ─────────────────────────────────────────────────────────────────────
_UA = ('Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) '
       'Chrome/124.0 Safari/537.36')
_SESSION_CACHE: dict[str, object] = {}


class Http:
    """Tiny JSON fetcher: in-process memo, retry with backoff, hard budget.

    Every network failure returns ``None`` rather than raising, so a partial
    outage degrades the site instead of breaking the build.
    """

    def __init__(self, timeout=15, retries=2, pause=0.12, budget_s=None):
        self.timeout = timeout
        self.retries = retries
        self.pause = pause
        self.budget_s = budget_s
        self.started = time.time()
        self.calls = 0
        self.failures = 0
        self.ctx = ssl.create_default_context()

    def out_of_budget(self):
        return self.budget_s is not None and (time.time() - self.started) > self.budget_s

    def get_json(self, url, cache=True):
        if cache and url in _SESSION_CACHE:
            return _SESSION_CACHE[url]
        if self.out_of_budget():
            return None
        last = None
        for attempt in range(self.retries + 1):
            try:
                req = urllib.request.Request(url, headers={
                    'User-Agent': _UA, 'Accept': 'application/json'})
                with urllib.request.urlopen(req, timeout=self.timeout,
                                            context=self.ctx) as resp:
                    data = json.loads(resp.read().decode('utf-8', 'replace'))
                self.calls += 1
                if cache:
                    _SESSION_CACHE[url] = data
                time.sleep(self.pause)
                return data
            except Exception as exc:          # noqa: BLE001 - never break the build
                last = exc
                if attempt < self.retries:
                    time.sleep(min(2 ** attempt * 0.5, 4.0) + random.random() * 0.2)
        self.failures += 1
        if cache:
            _SESSION_CACHE[url] = None
        return None


# ── dict digging ─────────────────────────────────────────────────────────────
def dig(obj, *path, default=None):
    """Walk nested dicts/lists safely: dig(d, 'a', 0, 'b')."""
    cur = obj
    for key in path:
        if cur is None:
            return default
        try:
            if isinstance(key, int):
                cur = cur[key]
            else:
                cur = cur.get(key)
        except (KeyError, IndexError, TypeError, AttributeError):
            return default
    return default if cur is None else cur


def first(seq, pred, default=None):
    for item in seq or []:
        try:
            if pred(item):
                return item
        except Exception:  # noqa: BLE001
            continue
    return default
