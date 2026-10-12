'use strict';

/* ==========================================================================
 * Engine — safe tokenizer + shunting-yard parser (no eval, ever)
 * ======================================================================== */

class CalcError extends Error {}

const FUNCTION_NAMES = ['sin', 'cos', 'tan', 'log', 'ln', '√'];
const PRECEDENCE = { '+': 2, '-': 2, '*': 3, '/': 3, 'u-': 4, '^': 5, '%': 6 };
const RIGHT_ASSOCIATIVE = new Set(['^', 'u-']);

function isDigitChar(c) {
  return c >= '0' && c <= '9';
}

/** Convert an expression string into tokens, inserting implicit multiplication. */
function tokenize(expr) {
  const tokens = [];
  let i = 0;
  const len = expr.length;

  while (i < len) {
    const c = expr[i];

    if (c === ' ') { i++; continue; }

    if (isDigitChar(c) || c === '.') {
      let num = '';
      while (i < len && (isDigitChar(expr[i]) || expr[i] === '.')) {
        num += expr[i];
        i++;
      }
      // scientific notation, e.g. "1e-7" coming from stored results
      if (expr[i] === 'e' && i + 1 < len) {
        let j = i + 1;
        let sign = '';
        if (expr[j] === '-' || expr[j] === '+') { sign = expr[j]; j++; }
        if (j < len && isDigitChar(expr[j])) {
          let exp = '';
          while (j < len && isDigitChar(expr[j])) { exp += expr[j]; j++; }
          num += 'e' + sign + exp;
          i = j;
        }
      }
      if (num === '.' || (num.match(/\./g) || []).length > 1) {
        throw new CalcError('Invalid number');
      }
      tokens.push({ type: 'num', value: parseFloat(num) });
      continue;
    }

    let matchedFunc = false;
    for (const fn of FUNCTION_NAMES) {
      if (expr.startsWith(fn, i)) {
        tokens.push({ type: 'func', value: fn === '√' ? 'sqrt' : fn });
        i += fn.length;
        matchedFunc = true;
        break;
      }
    }
    if (matchedFunc) continue;

    if (c === 'π') { tokens.push({ type: 'num', value: Math.PI }); i++; continue; }
    if (c === 'e') { tokens.push({ type: 'num', value: Math.E }); i++; continue; }
    if (c === '(') { tokens.push({ type: 'lp' }); i++; continue; }
    if (c === ')') { tokens.push({ type: 'rp' }); i++; continue; }
    if ('+-*/^%'.includes(c)) { tokens.push({ type: 'op', value: c }); i++; continue; }

    throw new CalcError('Invalid character "' + c + '"');
  }

  // Implicit multiplication: 2π, 3(4), (1+2)(3+4), 50%(1+1), 2sin(1) ...
  const out = [];
  for (let k = 0; k < tokens.length; k++) {
    const t = tokens[k];
    if (k > 0) {
      const p = tokens[k - 1];
      const pIsValue = p.type === 'num' || p.type === 'rp' || (p.type === 'op' && p.value === '%');
      const tStartsValue = t.type === 'num' || t.type === 'lp' || t.type === 'func';
      if (pIsValue && tStartsValue) out.push({ type: 'op', value: '*' });
    }
    out.push(t);
  }
  return out;
}

/** Shunting-yard: tokens -> Reverse Polish Notation. */
function toRPN(tokens) {
  const output = [];
  const stack = [];
  let prev = null;

  for (const t of tokens) {
    if (t.type === 'num') {
      output.push(t);
    } else if (t.type === 'func' || t.type === 'lp') {
      stack.push(t);
    } else if (t.type === 'rp') {
      let found = false;
      while (stack.length) {
        const top = stack.pop();
        if (top.type === 'lp') { found = true; break; }
        output.push(top);
      }
      if (!found) throw new CalcError('Mismatched parentheses');
      if (stack.length && stack[stack.length - 1].type === 'func') output.push(stack.pop());
    } else { // operator
      let opVal = t.value;
      const unary = opVal === '-' &&
        (!prev || prev.type === 'lp' || prev.type === 'func' ||
         (prev.type === 'op' && prev.value !== '%'));
      if (unary) opVal = 'u-';

      if (opVal === 'u-') {
        // unary minus binds to whatever follows; never pops the stack
        stack.push({ type: 'op', value: 'u-' });
      } else {
        while (stack.length) {
          const top = stack[stack.length - 1];
          if (top.type === 'func') { output.push(stack.pop()); continue; }
          if (top.type !== 'op') break;
          const tp = PRECEDENCE[top.value];
          const cp = PRECEDENCE[opVal];
          if (tp > cp || (tp === cp && !RIGHT_ASSOCIATIVE.has(opVal))) output.push(stack.pop());
          else break;
        }
        stack.push({ type: 'op', value: opVal });
      }
      prev = { type: 'op', value: opVal };
      continue;
    }
    prev = t;
  }

  while (stack.length) {
    const t = stack.pop();
    if (t.type === 'lp' || t.type === 'rp') throw new CalcError('Mismatched parentheses');
    output.push(t);
  }
  return output;
}

function applyOperator(op, a, b) {
  switch (op) {
    case '+': return a + b;
    case '-': return a - b;
    case '*': return a * b;
    case '/':
      if (b === 0) throw new CalcError("Can't divide by zero");
      return a / b;
    case '^': {
      const r = Math.pow(a, b);
      if (Number.isNaN(r)) throw new CalcError('Invalid power operation');
      return r;
    }
    default: throw new CalcError('Unknown operator');
  }
}

function applyFunction(name, x, angleMode) {
  const toRad = angleMode === 'deg' ? (x * Math.PI) / 180 : x;
  switch (name) {
    case 'sin': return Math.sin(toRad);
    case 'cos': return Math.cos(toRad);
    case 'tan': {
      if (angleMode === 'deg' && Math.abs((((x % 180) + 180) % 180) - 90) < 1e-9) {
        throw new CalcError('tan is undefined for this angle');
      }
      return Math.tan(toRad);
    }
    case 'sqrt':
      if (x < 0) throw new CalcError("Can't take √ of a negative number");
      return Math.sqrt(x);
    case 'ln':
      if (x <= 0) throw new CalcError('ln needs a positive number');
      return Math.log(x);
    case 'log':
      if (x <= 0) throw new CalcError('log needs a positive number');
      return Math.log10(x);
    default: throw new CalcError('Unknown function');
  }
}

function evalRPN(rpn, angleMode) {
  const st = [];
  for (const t of rpn) {
    if (t.type === 'num') {
      st.push(t.value);
    } else if (t.type === 'func') {
      if (st.length < 1) throw new CalcError('Invalid expression');
      st.push(applyFunction(t.value, st.pop(), angleMode));
    } else if (t.value === 'u-') {
      if (st.length < 1) throw new CalcError('Invalid expression');
      st.push(-st.pop());
    } else if (t.value === '%') {
      if (st.length < 1) throw new CalcError('Invalid expression');
      st.push(st.pop() / 100);
    } else {
      if (st.length < 2) throw new CalcError('Invalid expression');
      const b = st.pop();
      const a = st.pop();
      st.push(applyOperator(t.value, a, b));
    }
  }
  if (st.length !== 1) throw new CalcError('Invalid expression');
  return st[0];
}

/** Kill floating-point noise: 0.1 + 0.2 -> 0.3 */
function roundValue(n) {
  if (n === 0) return 0;
  return parseFloat(n.toPrecision(12));
}

/** Full pipeline: expression string -> rounded number (throws CalcError). */
function evaluate(expression, angleMode) {
  const mode = angleMode === 'rad' ? 'rad' : 'deg';
  const value = evalRPN(toRPN(tokenize(expression)), mode);
  if (typeof value !== 'number' || Number.isNaN(value)) throw new CalcError('Invalid expression');
  if (!Number.isFinite(value)) throw new CalcError('Overflow — number is too large');
  return roundValue(value);
}

/* ==========================================================================
 * Formatting helpers (pure)
 * ======================================================================== */

/** Pretty number with thousands separators (for the display). */
function formatNumber(value) {
  if (!Number.isFinite(value)) return 'Error';
  const n = parseFloat(value.toPrecision(12));
  if (n === 0) return '0';
  const abs = Math.abs(n);
  if (abs >= 1e12 || abs < 1e-9) {
    return n.toExponential(7).replace(/\.?0+e/, 'e');
  }
  const negative = n < 0;
  const parts = String(abs).split('.');
  const intFmt = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, ',');
  return (negative ? '-' : '') + intFmt + (parts[1] ? '.' + parts[1] : '');
}

/** Machine-readable string that the tokenizer can parse back. */
function numberToRaw(value) {
  return String(roundValue(value));
}

/** Pretty expression with × ÷ − and thousands separators (for the display). */
function formatExpression(expr) {
  const pretty = expr
    .replace(/\*/g, ' × ')
    .replace(/\//g, ' ÷ ')
    .replace(/-/g, '−');
  return pretty.replace(/\d+(?:\.\d*)?(?:e[+−-]?\d+)?/g, (m) => {
    if (m.includes('e')) return m;
    const parts = m.split('.');
    const intFmt = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, ',');
    return parts.length > 1 ? intFmt + '.' + parts.slice(1).join('.') : intFmt;
  });
}

/* ==========================================================================
 * UI (browser only)
 * ======================================================================== */

if (typeof document !== 'undefined') {
  initCalculatorUI();
}

function initCalculatorUI() {
  const root = document.documentElement;
  const displayEl = document.querySelector('.display');
  const expressionEl = document.getElementById('expression');
  const resultEl = document.getElementById('result');
  const memoryIndicator = document.getElementById('memoryIndicator');
  const sciPanel = document.getElementById('sciPanel');
  const sciToggle = document.getElementById('sciToggle');
  const angleToggle = document.getElementById('angleToggle');
  const themeToggle = document.getElementById('themeToggle');
  const historyPanel = document.getElementById('historyPanel');
  const historyList = document.getElementById('historyList');
  const historyEmpty = document.getElementById('historyEmpty');
  const toastEl = document.getElementById('toast');

  let expression = '';
  let lastExpression = '';
  let lastResult = 0;
  let justEvaluated = false;
  let errorMsg = '';
  let memory = 0;
  let angleMode = 'deg';
  const history = [];
  let toastTimer = null;

  /* ---------- Theme ---------- */
  function setTheme(theme) {
    root.dataset.theme = theme;
    try { localStorage.setItem('calc-theme', theme); } catch (e) { /* private mode */ }
    themeToggle.textContent = theme === 'dark' ? '☀️' : '🌙';
  }
  let initialTheme = 'dark';
  try {
    initialTheme = localStorage.getItem('calc-theme') ||
      (window.matchMedia && window.matchMedia('(prefers-color-scheme: light)').matches ? 'light' : 'dark');
  } catch (e) { /* ignore */ }
  setTheme(initialTheme);

  try {
    angleMode = localStorage.getItem('calc-angle') === 'rad' ? 'rad' : 'deg';
  } catch (e) { /* ignore */ }

  /* ---------- Toast ---------- */
  function toast(msg) {
    toastEl.textContent = msg;
    toastEl.classList.add('show');
    clearTimeout(toastTimer);
    toastTimer = setTimeout(() => toastEl.classList.remove('show'), 1600);
  }

  /* ---------- Rendering ---------- */
  function computePreview() {
    if (!expression || !/[0-9)πe%]$/.test(expression)) return '';
    if (!/[+\-*/^%()]|sin|cos|tan|log|ln|√/.test(expression)) return '';
    try {
      return '≈ ' + formatNumber(evaluate(expression, angleMode));
    } catch (err) {
      return '';
    }
  }

  function render() {
    if (errorMsg) {
      expressionEl.textContent = formatExpression(expression);
      resultEl.textContent = errorMsg;
      resultEl.classList.add('error');
    } else {
      resultEl.classList.remove('error');
      if (justEvaluated) {
        expressionEl.textContent = formatExpression(lastExpression) + ' =';
        resultEl.textContent = formatNumber(lastResult);
      } else {
        expressionEl.textContent = computePreview();
        resultEl.textContent = expression ? formatExpression(expression) : '0';
      }
    }
    const len = resultEl.textContent.length;
    resultEl.classList.toggle('fs-md', len > 12 && len <= 20);
    resultEl.classList.toggle('fs-sm', len > 20);
    memoryIndicator.hidden = memory === 0;
    angleToggle.textContent = angleMode.toUpperCase();
    expressionEl.scrollLeft = expressionEl.scrollWidth;
  }

  function flashError(msg) {
    errorMsg = msg;
    render();
    displayEl.classList.remove('shake');
    void displayEl.offsetWidth; // restart animation
    displayEl.classList.add('shake');
  }

  /* ---------- History ---------- */
  function renderHistory() {
    historyList.innerHTML = '';
    historyEmpty.style.display = history.length ? 'none' : '';
    for (const item of history) {
      const li = document.createElement('li');
      li.className = 'history-item';
      li.tabIndex = 0;
      li.title = 'Click to reuse this result';
      const exprSpan = document.createElement('span');
      exprSpan.className = 'history-expr';
      exprSpan.textContent = formatExpression(item.expr) + ' =';
      const resultSpan = document.createElement('span');
      resultSpan.className = 'history-result';
      resultSpan.textContent = formatNumber(item.result);
      li.appendChild(exprSpan);
      li.appendChild(resultSpan);
      const reuse = () => insertRawNumber(numberToRaw(item.result));
      li.addEventListener('click', reuse);
      li.addEventListener('keydown', (e) => {
        if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); reuse(); }
      });
      historyList.appendChild(li);
    }
  }

  function addHistory(expr, result) {
    history.unshift({ expr, result });
    if (history.length > 50) history.pop();
    renderHistory();
  }

  /* ---------- Input actions ---------- */
  function insertDigit(d) {
    errorMsg = '';
    if (justEvaluated) { expression = ''; justEvaluated = false; }
    const tail = expression.match(/[\d.]*$/)[0];
    if (tail.replace('.', '').length >= 15) return;
    if (tail === '0') expression = expression.slice(0, -1) + d;
    else expression += d;
    render();
  }

  function insertDot() {
    errorMsg = '';
    if (justEvaluated) { expression = '0.'; justEvaluated = false; render(); return; }
    const tail = expression.match(/[\d.]*$/)[0];
    if (tail.includes('.')) return;
    expression += tail ? '.' : '0.';
    render();
  }

  function insertOperator(op) {
    errorMsg = '';
    if (justEvaluated) {
      expression = numberToRaw(lastResult); // chained calculation
      justEvaluated = false;
    }
    if (!expression) {
      if (op === '-') expression = '-';
      render();
      return;
    }
    const last = expression.slice(-1);
    if ('+-*/^'.includes(last)) {
      if (op === '-' && last !== '-') {
        expression += '-'; // negative operand, e.g. 5×-3
      } else {
        while (expression.length && '+-*/^'.includes(expression.slice(-1))) {
          expression = expression.slice(0, -1);
        }
        if (!expression) { if (op === '-') expression = '-'; }
        else expression += op;
      }
    } else if (last === '(') {
      if (op === '-') expression += '-';
    } else {
      expression += op;
    }
    render();
  }

  function insertLParen() {
    errorMsg = '';
    if (justEvaluated) { expression = ''; justEvaluated = false; }
    expression += '(';
    render();
  }

  function insertRParen() {
    errorMsg = '';
    if (justEvaluated) return;
    const open = (expression.match(/\(/g) || []).length;
    const close = (expression.match(/\)/g) || []).length;
    if (open > close && /[\d)πe%]$/.test(expression)) {
      expression += ')';
      render();
    }
  }

  function insertFunc(fn) {
    errorMsg = '';
    if (justEvaluated) { expression = ''; justEvaluated = false; }
    expression += fn + '(';
    render();
  }

  function insertConstant(c) {
    errorMsg = '';
    if (justEvaluated) { expression = ''; justEvaluated = false; }
    expression += c;
    render();
  }

  function percent() {
    errorMsg = '';
    if (justEvaluated) { expression = numberToRaw(lastResult); justEvaluated = false; }
    if (/[\d)πe]$/.test(expression)) expression += '%';
    render();
  }

  function square() {
    errorMsg = '';
    if (justEvaluated) { expression = numberToRaw(lastResult); justEvaluated = false; }
    if (/[\d)πe%]$/.test(expression)) expression += '^2';
    render();
  }

  function toggleSign() {
    errorMsg = '';
    if (justEvaluated) { expression = numberToRaw(lastResult); justEvaluated = false; }
    let m = expression.match(/\(-(\d+(?:\.\d+)?)\)$/);
    if (m) {
      const before = expression.slice(0, m.index);
      expression = /[\d)πe%]$/.test(before) ? before + '(' + m[1] + ')' : before + m[1];
      render();
      return;
    }
    m = expression.match(/(\d+(?:\.\d+)?)$/);
    if (m) {
      expression = expression.slice(0, m.index) + '(-' + m[1] + ')';
    }
    render();
  }

  function backspace() {
    errorMsg = '';
    if (justEvaluated) { justEvaluated = false; render(); return; }
    const funcs = ['sin(', 'cos(', 'tan(', 'log(', 'ln(', '√('];
    for (const f of funcs) {
      if (expression.endsWith(f)) {
        expression = expression.slice(0, -f.length);
        render();
        return;
      }
    }
    expression = expression.slice(0, -1);
    render();
  }

  function clearEntry() {
    errorMsg = '';
    if (justEvaluated) { justEvaluated = false; render(); return; }
    expression = expression
      .replace(/(?:sin|cos|tan|log|ln|√)\($/, '')
      .replace(/\(-\d+(?:\.\d+)?\)$/, '')
      .replace(/[\d.]+%?$/, '')
      .replace(/[πe]$/, '');
    render();
  }

  function clearAll() {
    expression = '';
    lastExpression = '';
    justEvaluated = false;
    errorMsg = '';
    render();
  }

  function onEquals() {
    if (!expression) return;
    if (!/[0-9)πe%]$/.test(expression)) {
      flashError('Incomplete expression');
      return;
    }
    try {
      const result = evaluate(expression, angleMode);
      addHistory(expression, result);
      lastExpression = expression;
      lastResult = result;
      justEvaluated = true;
      errorMsg = '';
    } catch (err) {
      flashError(err instanceof CalcError ? err.message : 'Error');
    }
    render();
  }

  /* ---------- Memory ---------- */
  function currentValue() {
    if (justEvaluated) return lastResult;
    if (!expression) return 0;
    if (!/[0-9)πe%]$/.test(expression)) return null;
    try { return evaluate(expression, angleMode); } catch (err) { return null; }
  }

  function insertRawNumber(raw) {
    errorMsg = '';
    if (justEvaluated) { expression = ''; justEvaluated = false; }
    if (!raw) return;
    const lastIsValue = /[\d)πe%]$/.test(expression);
    if (raw.startsWith('-') && lastIsValue) expression += '*(' + raw + ')';
    else expression += raw;
    render();
  }

  function memoryAdd(sign) {
    const v = currentValue();
    if (v === null) { toast('Nothing to store'); return; }
    memory = roundValue(memory + sign * v);
    toast(sign > 0 ? 'Added to memory' : 'Subtracted from memory');
    render();
  }

  /* ---------- Copy ---------- */
  async function copyResult() {
    const text = errorMsg ? '' : (justEvaluated ? numberToRaw(lastResult) : (expression || '0'));
    if (!text) return;
    try {
      await navigator.clipboard.writeText(text);
      toast('Copied to clipboard');
    } catch (err) {
      const ta = document.createElement('textarea');
      ta.value = text;
      ta.style.position = 'fixed';
      ta.style.opacity = '0';
      document.body.appendChild(ta);
      ta.select();
      try {
        document.execCommand('copy');
        toast('Copied to clipboard');
      } catch (e2) {
        toast('Copy failed');
      }
      ta.remove();
    }
  }

  /* ---------- Event wiring ---------- */
  document.addEventListener('click', (e) => {
    const btn = e.target.closest('[data-action]');
    if (!btn) return;
    const { action, value } = btn.dataset;
    switch (action) {
      case 'digit': insertDigit(value); break;
      case 'dot': insertDot(); break;
      case 'op': insertOperator(value); break;
      case 'percent': percent(); break;
      case 'square': square(); break;
      case 'lparen': insertLParen(); break;
      case 'rparen': insertRParen(); break;
      case 'func': insertFunc(value); break;
      case 'const': insertConstant(value); break;
      case 'sign': toggleSign(); break;
      case 'ac': clearAll(); break;
      case 'ce': clearEntry(); break;
      case 'back': backspace(); break;
      case 'equals': onEquals(); break;
      case 'copy': copyResult(); break;
      case 'mc': memory = 0; toast('Memory cleared'); render(); break;
      case 'mr': insertRawNumber(numberToRaw(memory)); break;
      case 'mplus': memoryAdd(1); break;
      case 'mminus': memoryAdd(-1); break;
      case 'theme': setTheme(root.dataset.theme === 'dark' ? 'light' : 'dark'); break;
      case 'sci': {
        const open = sciPanel.hidden;
        sciPanel.hidden = !open;
        sciToggle.classList.toggle('active', open);
        sciToggle.setAttribute('aria-expanded', String(open));
        break;
      }
      case 'angle': {
        angleMode = angleMode === 'deg' ? 'rad' : 'deg';
        try { localStorage.setItem('calc-angle', angleMode); } catch (err) { /* ignore */ }
        render();
        break;
      }
      case 'hist-clear': history.length = 0; renderHistory(); break;
      case 'hist-toggle': historyPanel.classList.toggle('open'); break;
      default: break;
    }
  });

  document.addEventListener('keydown', (e) => {
    if (e.ctrlKey || e.metaKey || e.altKey) return;
    if (e.target.closest && e.target.closest('.history-item')) return;
    const k = e.key;
    let handled = true;
    if (/^[0-9]$/.test(k)) insertDigit(k);
    else if (k === '.' || k === ',') insertDot();
    else if (k === '+' || k === '-' || k === '*' || k === '/' || k === '^') insertOperator(k);
    else if (k === '%') percent();
    else if (k === '(') insertLParen();
    else if (k === ')') insertRParen();
    else if (k === 'Enter' || k === '=') onEquals();
    else if (k === 'Escape') clearAll();
    else if (k === 'Backspace') backspace();
    else if (k === 'Delete') clearEntry();
    else handled = false;
    if (handled) e.preventDefault();
  });

  renderHistory();
  render();
}
