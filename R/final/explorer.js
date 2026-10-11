// Eligibility explorer (@fig-explorer). Attached with htmlwidgets::onRender()
// by eligibility_explorer() in R/final/explorer.R: `el` holds the figure
// (the waterfall on top, 100 pupils as icons below), and `data` the school
// model, the schools, and the Netherlands. Groups of schools are computed
// here from their schools.
// The controls choose the unit (the Netherlands, a province, municipality,
// school board, group of similar schools, or school), the pupils (all, or
// those with one provisional advice), and what-ifs: another test or, for a
// school, the teachers of another school. The figure is filled with
// Plotly.react().
function (el, x, data) {
  const { model, schools, national, labels, style } = data;
  const sources = ["teacher_error", "leniency", "test_error", "test_menu"];
  const all = (1 << sources.length) - 1;
  const schoolYear = 2025;

  // ---------- school model (mirrors R/school-model/school-model.R) ----------

  // Standard normal distribution function (Abramowitz and Stegun 7.1.26).
  function pnorm(z) {
    const t = 1 / (1 + 0.3275911 * Math.abs(z) / Math.SQRT2);
    const erf = 1 - ((((1.061405429 * t - 1.453152027) * t + 1.421413741) *
      t - 0.284496736) * t + 0.254829592) * t * Math.exp(-z * z / 2);
    return z >= 0 ? 0.5 * (1 + erf) : 0.5 * (1 - erf);
  }

  const grid = Array.from({ length: model.grid.n }, (_, k) =>
    model.grid.from + k * (model.grid.to - model.grid.from) / (model.grid.n - 1));

  // Probability of each category for readings with the given means and sd;
  // with sd = 0 the reading is exact (category_probabilities() in R).
  function categories(thresholds, means, sd) {
    return means.map(mean => {
      const below = [0, ...thresholds.map(edge =>
        sd === 0 ? (mean <= edge ? 1 : 0) : pnorm((edge - mean) / sd)), 1];
      return below.slice(1).map((value, k) => value - below[k]);
    });
  }

  // matrices that do not depend on the school are computed once
  const memo = new Map();
  const remember = (key, compute) => {
    if (!memo.has(key)) memo.set(key, compute());
    return memo.get(key);
  };
  const versions = Object.fromEntries(model.versions.map(v => [v.test, v]));
  const single = test => [{ ...versions[test], weight: 1 }];
  const providerTest = provider => model.mixtures[provider].map(m =>
    ({ ...versions[m.test], weight: m.weight }));
  const reference = single(model.reference);

  // Probability of each test result for every theta of the grid.
  function testResults(mixture, error) {
    const key = mixture.map(m => `${m.test}:${m.weight}`).join(",") + `|${error}`;
    return remember(key, () => {
      const parts = mixture.map(m => categories(
        model.tau, grid.map(t => m.alpha * t + m.beta), error ? m.sd_T : 0
      ).map(row => row.map(p => m.weight * p)));
      return parts.reduce((sum, part) =>
        sum.map((row, g) => row.map((p, j) => p + part[g][j])));
    });
  }

  // Probability of each provisional advice for every theta of the grid.
  function teacherAdvice(delta, error) {
    const compute = () => categories(
      model.lambda, grid.map(t => t + delta), error ? model.teacher_sd : 0
    );
    return delta === 0 ? remember(`teacher|${error}`, compute) : compute();
  }

  const emptyCells = () => labels.provisional.map(() => labels.results.map(() => 0));

  // The cells of a school (shares per provisional advice and test result)
  // for every set of switched-on sources (a bit mask over `sources`), with
  // optional what-ifs: another teacher's leniency, another test.
  function schoolCells(school, whatIf) {
    const delta = whatIf.delta ?? school.delta;
    const test = whatIf.test ? single(whatIf.test) : providerTest(school.provider);
    const density = grid.map(t => Math.exp(-0.5 * ((t - school.mu) / school.sigma) ** 2));
    const total = density.reduce((a, b) => a + b, 0);
    const lenient = [teacherAdvice(delta, false), teacherAdvice(delta, true)];
    return Array.from({ length: all + 1 }, (_, mask) => {
      const has = name => (mask & (1 << sources.indexOf(name))) !== 0;
      const teacher = has("leniency")
        ? lenient[+has("teacher_error")] : teacherAdvice(0, has("teacher_error"));
      const result = testResults(has("test_menu") ? test : reference, has("test_error"));
      const table = emptyCells();
      grid.forEach((_, g) => {
        const weight = density[g] / total;
        teacher[g].forEach((p, i) => {
          if (p > 1e-12) result[g].forEach((q, j) => { table[i][j] += weight * p * q; });
        });
      });
      return table;
    });
  }

  // Pupil-weighted cells of a group of schools, for every mask.
  function groupCells(members, whatIf) {
    const pupils = members.reduce((a, k) => a + schools.pupils[k], 0);
    const cells = Array.from({ length: all + 1 }, emptyCells);
    members.forEach(k => {
      const weight = schools.pupils[k] / pupils;
      schoolCells(school(k), whatIf).forEach((table, mask) => table.forEach((row, i) =>
        row.forEach((p, j) => { cells[mask][i][j] += weight * p; })));
    });
    return cells;
  }

  // Share of eligible pupils among all pupils, or among the pupils with
  // one provisional advice (eligibility_among() in R).
  function eligibility(cells, advice) {
    const rows = advice === "all" ? cells.map((_, i) => i) : [labels.provisional_keys.indexOf(advice)];
    let eligible = 0;
    let total = 0;
    rows.forEach(i => cells[i].forEach((p, j) => {
      total += p;
      if (data.gap[i][j] > 0) eligible += p;
    }));
    return advice === "all" ? eligible : eligible / total;
  }

  // Shapley values over `sources` (shapley_values() in R).
  const factorial = n => (n <= 1 ? 1 : n * factorial(n - 1));
  function decompose(cells, advice) {
    const value = mask => eligibility(cells[mask], advice);
    const result = { category_design: value(0), eligible: value(all) };
    sources.forEach((source, k) => {
      let sum = 0;
      for (let mask = 0; mask <= all; mask++) {
        if (mask & (1 << k)) continue;
        const size = sources.filter((_, b) => mask & (1 << b)).length;
        sum += factorial(size) * factorial(sources.length - size - 1) /
          factorial(sources.length) * (value(mask | (1 << k)) - value(mask));
      }
      result[source] = sum;
    });
    return result;
  }

  // ---------- units ----------

  const school = k => Object.fromEntries(Object.keys(schools).map(key => [key, schools[key][k]]));
  const sum = (members, get) => members.reduce((a, k) => a + get(k), 0);

  // Groups of schools per level, in order of their first school, with their
  // pupils, observed shares, and tests (pupil-weighted over their schools).
  const groups = [];
  ["province", "municipality", "board", "similar"].forEach(level => {
    const byName = new Map();
    schools[level].forEach((name, k) => {
      if (name == null) return;
      if (!byName.has(name)) byName.set(name, []);
      byName.get(name).push(k);
    });
    byName.forEach((members, name) => {
      const pupils = sum(members, k => schools.pupils[k]);
      const higher = sum(members, k => schools.higher[k] * schools.pupils[k]);
      const perTest = new Map();
      [...new Set(members.map(k => schools.provider[k]))].sort().forEach(test => perTest.set(test, 0));
      members.forEach(k => perTest.set(schools.provider[k], perTest.get(schools.provider[k]) + schools.pupils[k] / pupils));
      const tests = [...perTest].sort((a, b) => b[1] - a[1])
        .map(([test, share]) => `${test} ${share < 0.005 ? "<1" : Math.round(100 * share)}%`).join(", ");
      groups.push({
        level, name, members, schools: members.length, pupils, higher: higher / pupils,
        // no raised share without pupils with a higher test advice
        raised: higher > 0 ? sum(members, k => schools.raised_pupils[k]) / higher : null,
        tests
      });
    });
  });
  const groupIndex = new Map(groups.map((g, k) => [`${g.level}|${g.name}`, k]));
  // the cells of a group with its own tests, computed when first needed
  const ownCells = new Map();
  const cellsOf = k => {
    if (!ownCells.has(k)) ownCells.set(k, groupCells(groups[k].members, {}));
    return ownCells.get(k);
  };
  const levelName = {
    province: "province", municipality: "municipality", board: "school board",
    similar: "similar schools"
  };
  const groupLabel = g => (g.level === "similar" ? `Schools with a ${g.name}` : g.name);
  const adviceLabel = advice => labels.provisional[labels.provisional_keys.indexOf(advice)];

  // every unit for the search box, with a short description
  const plain = text => String(text).normalize("NFD").replace(/[̀-ͯ]/g, "").toLowerCase();
  const units = [
    { kind: "national", label: "the Netherlands", detail: "all schools", rank: 0, pupils: Infinity },
    ...groups.map((g, k) => ({
      kind: "group", index: k, label: groupLabel(g),
      detail: `${levelName[g.level]} · ${g.schools} schools`, rank: 1, pupils: g.pupils
    })),
    ...schools.name.map((name, k) => ({
      kind: "school", index: k, label: name,
      detail: `school in ${schools.place[k]} · ${schools.provider[k]}`, rank: 2,
      pupils: schools.pupils[k], extra: `${schools.municipality[k]} ${schools.board[k]}`
    }))
  ].map(u => ({ ...u, text: plain(`${u.label} ${u.detail} ${u.extra || ""}`), name: plain(u.label) }));

  // units whose text contains every word of the query, best matches first
  function search(query, kinds) {
    const words = plain(query).split(/\s+/).filter(Boolean);
    if (!words.length) return [];
    const phrase = words.join(" ");
    const score = u => (u.name.startsWith(phrase) ? 0 : u.name.includes(words[0]) ? 1 : 2);
    return units
      .filter(u => kinds.includes(u.kind) && words.every(w => u.text.includes(w)))
      .sort((a, b) => score(a) - score(b) || a.rank - b.rank || b.pupils - a.pupils)
      .slice(0, 10);
  }

  const state = {
    unit: { kind: "national" }, year: Math.max(...national.pupils.map(d => d.test_year)),
    advice: "all", teacher: null, test: ""
  };

  // highest point of a waterfall, for the scale
  const peak = d => Math.max(d.eligible, d.category_design,
    d.category_design + Math.max(0, d.teacher_error) + Math.max(0, d.leniency ?? 0) +
      Math.max(0, d.test_error));

  // 100 pupils by their test result: for one provisional advice, one entry
  // per test result; for all pupils, one entry per comparison
  function outcomesOf(table) {
    if (state.advice !== "all") {
      const i = labels.provisional_keys.indexOf(state.advice);
      const total = table[i].reduce((a, b) => a + b, 0);
      return labels.results.map((label, j) => ({
        label, share: total > 0 ? table[i][j] / total : 0, gap: data.gap[i][j]
      }));
    }
    // the title of an outcome says higher or lower, so the label only says how far
    const distance = gap => (gap === 0 ? "the same" : Math.abs(gap) < 1 ? "half a level" : "a level or more");
    const total = table.flat().reduce((a, b) => a + b, 0);
    return Object.keys(labels.relations).map(Number).sort((a, b) => a - b).map(gap => ({
      label: distance(gap), gap,
      share: table.reduce((sum, row, i) => sum + row.reduce((s, p, j) =>
        s + (data.gap[i][j] === gap ? p : 0), 0), 0) / total
    }));
  }

  // Everything that the figure shows for the current state.
  function view() {
    const unit = state.unit;
    const active = state.teacher != null || Boolean(state.test);
    if (unit.kind === "national") {
      const rows = national.decomposition.filter(d => d.test_year === state.year && d.advice === state.advice);
      const chosen = state.test || "national average";
      const row = rows.find(d => d.test === chosen);
      const average = rows.find(d => d.test === "national average");
      // observed test results of the CvTE tables, weighted by the provisional advice
      const provisional = national.provisional.filter(d => d.test_year === state.year && d.test === chosen);
      const results = national.results.filter(d => d.test_year === state.year && d.test === chosen);
      const table = labels.provisional_keys.map(p => {
        const weight = (provisional.find(d => d.provisional === p) || { share: 0 }).share;
        return labels.result_keys.map(r =>
          weight * (results.find(d => d.provisional === p && d.test_advice === r) || { share: 0 }).share);
      });
      const observed = state.test ? null : state.advice === "all"
        ? (national.observed.find(d => d.test_year === state.year) || {}).higher
        : eligibility(table, state.advice);
      return {
        kind: "national", name: "the Netherlands",
        pupils: national.pupils.find(d => d.test_year === state.year).pupils,
        decomposition: { ...row, leniency: null },
        asIs: state.test ? average.eligible : null, observed,
        top: Math.max(...rows.map(peak), observed || 0),
        tests: rows.filter(d => d.test !== "national average").map(d => d.test),
        outcomes: outcomesOf(table), estimated: false,
        raised: national.raised.share, raisedWhere: "",
        whatIf: state.test ? `if all pupils took ${state.test}` : ""
      };
    }
    const tests = model.versions.map(v => v.test);
    if (unit.kind === "school") {
      const s = school(unit.index);
      const now = schoolCells(s, {});
      const result = active ? schoolCells(s, {
        delta: state.teacher == null ? undefined : schools.delta[state.teacher],
        test: state.test || undefined
      }) : now;
      const decomposition = decompose(result, state.advice);
      const before = decompose(now, state.advice);
      return {
        kind: "school", name: s.name, school: s, pupils: s.pupils,
        decomposition, asIs: active ? before.eligible : null,
        observed: active || state.advice !== "all" ? null : s.higher,
        top: Math.max(peak(before), peak(decomposition), state.advice === "all" ? s.higher : 0),
        tests, outcomes: outcomesOf(result[all]), estimated: true,
        raised: s.raised, raisedWhere: " at this school",
        whatIf: [
          state.test ? `with ${state.test}` : "",
          state.teacher == null ? "" : `with the teachers of ${schools.name[state.teacher]}`
        ].filter(Boolean).join(" and ")
      };
    }
    const g = groups[unit.index];
    const now = cellsOf(unit.index);
    const result = state.test ? groupCells(g.members, { test: state.test }) : now;
    const decomposition = decompose(result, state.advice);
    const before = decompose(now, state.advice);
    const observed = state.test || state.advice !== "all" ? null : g.higher;
    return {
      kind: "group", name: g.name, group: g, pupils: g.pupils,
      decomposition, asIs: state.test ? before.eligible : null, observed,
      top: Math.max(peak(before), peak(decomposition), observed ?? 0),
      tests, outcomes: outcomesOf(result[all]), estimated: true,
      raised: g.raised, raisedWhere: "",
      whatIf: state.test ? `if all pupils took ${state.test}` : ""
    };
  }

  // ---------- text ----------

  const escape = text => String(text).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  const wrap = (text, width) => {
    const lines = [];
    text.split(" ").forEach(word => {
      const last = lines[lines.length - 1];
      if (last !== undefined && (last + " " + word).length <= width) lines[lines.length - 1] = last + " " + word;
      else lines.push(word);
    });
    return lines.join("<br>");
  };
  const number = new Intl.NumberFormat("en-GB");
  const percent = value => `${Math.round(100 * value)}%`;
  const signed = value => {
    const rounded = Math.round(100 * Math.abs(value));
    return rounded === 0 ? "0%" : `${value < 0 ? "−" : "+"}${rounded}%`;
  };
  const title = (headline, subtitle) => `<b>${headline}</b>` + (subtitle
    ? `<br><span style='font-size:12px;color:${style.ink.secondary}'>${subtitle}</span>` : "");

  // where the pupils are, for the headline
  function place(v) {
    if (v.kind === "national") return "in the Netherlands";
    if (v.kind === "school") return `at ${escape(v.name)}`;
    const level = v.group.level;
    if (level === "board") return `at the schools of ${escape(v.name)}`;
    if (level === "similar") return `at schools with a ${escape(v.name)}`;
    return `in ${escape(v.name)}`;
  }
  const pupilsWord = () => (state.advice === "all" ? "the pupils" : `the pupils with a ${adviceLabel(state.advice)} advice`);

  function headline(v) {
    const d = v.decomposition;
    if (data.title) return data.title;
    if (v.asIs != null) {
      return wrap(`${percent(d.eligible)} instead of ${percent(v.asIs)} of ${pupilsWord()} ${place(v)} would be eligible ${escape(v.whatIf)}`, 62);
    }
    if (v.kind === "national" && state.advice === "all") {
      return wrap(`Even with a perfect teacher and a perfect test, ${Math.round(10 * d.category_design / d.eligible)} in 10 eligible pupils would still be eligible`, 62);
    }
    return wrap(`${percent(d.eligible)} of ${pupilsWord()} ${place(v)} are eligible for a higher track`, 62);
  }

  // the leniency of a school's teachers, in words
  function verdict(s) {
    const z = s.delta / s.se_delta;
    if (z <= -2) return "stricter than the national teacher: they advise lower than the test results suggest";
    if (z >= 2) return "more lenient than the national teacher: they advise higher than the test results suggest";
    return "not clearly different from the national teacher";
  }

  // ---------- controls ----------

  if (!document.getElementById("eligibility-explorer-style")) {
    const css = document.createElement("style");
    css.id = "eligibility-explorer-style";
    const icon = "data:image/svg+xml," + encodeURIComponent(
      `<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='none' stroke='${style.ink.muted}' stroke-width='1.6'><circle cx='7' cy='7' r='5'/><path d='M11 11l4 4'/></svg>`);
    css.textContent = `
      .ee { text-align: left; margin: 0.5rem 0 0.75rem; font-size: 0.95rem; }
      .ee-search { position: relative; max-width: 36rem; }
      .ee-search input { width: 100%; box-sizing: border-box; padding: 0.5rem 0.75rem 0.5rem 2.1rem;
        border: 1px solid ${style.ink.axis}; border-radius: 0.5rem; font: inherit;
        background: #fff url("${icon}") no-repeat 0.7rem center / 1rem; }
      .ee-search input:focus { outline: 2px solid #2a78d6; outline-offset: 0; border-color: transparent; }
      .ee-results { position: absolute; z-index: 30; left: 0; right: 0; top: calc(100% + 4px);
        background: #fff; border: 1px solid ${style.ink.shade}; border-radius: 0.5rem;
        box-shadow: 0 6px 18px rgba(11, 11, 11, 0.12); max-height: 22rem; overflow-y: auto; }
      .ee-result { display: flex; justify-content: space-between; gap: 1rem; padding: 0.45rem 0.75rem; cursor: pointer; }
      .ee-result[aria-selected="true"], .ee-result:hover { background: ${style.ink.shade}; }
      .ee-result small { color: ${style.ink.secondary}; white-space: nowrap; }
      .ee-empty { padding: 0.45rem 0.75rem; color: ${style.ink.muted}; }
      .ee-path { display: flex; flex-wrap: wrap; align-items: center; gap: 0.3rem; margin: 0.6rem 0 0.2rem; font-size: 0.85rem; }
      .ee-path button { border: 0; background: none; padding: 0; color: #2a78d6; cursor: pointer; font: inherit;
        text-decoration: underline; text-underline-offset: 2px; }
      .ee-path .ee-current { font-weight: 600; }
      .ee-path .ee-separator { color: ${style.ink.axis}; }
      .ee-card { border: 1px solid ${style.ink.shade}; border-radius: 0.6rem; padding: 0.6rem 0.8rem; margin: 0.4rem 0; background: #fcfcfb; }
      .ee-card .ee-name { font-weight: 600; }
      .ee-card .ee-detail { color: ${style.ink.secondary}; font-size: 0.88rem; }
      .ee-row { display: flex; flex-wrap: wrap; align-items: center; gap: 0.4rem 0.6rem; margin: 0.45rem 0; }
      .ee-label { font-weight: 600; font-size: 0.88rem; min-width: 8.5rem; }
      .ee-segmented { display: inline-flex; flex-wrap: wrap; gap: 2px; padding: 2px; border-radius: 0.5rem; background: ${style.ink.shade}; }
      .ee-segmented button { border: 0; background: transparent; padding: 0.2rem 0.6rem; border-radius: 0.4rem;
        font: inherit; font-size: 0.85rem; color: ${style.ink.secondary}; cursor: pointer; }
      .ee-segmented button[aria-pressed="true"] { background: #fff; color: ${style.ink.primary}; font-weight: 600;
        box-shadow: 0 1px 2px rgba(11, 11, 11, 0.15); }
      .ee-whatif { border-left: 3px solid ${style.ink.axis}; padding: 0.2rem 0 0.2rem 0.8rem; margin: 0.6rem 0; }
      .ee-whatif.ee-active { border-left-color: #4a3aa7; }
      .ee-whatif .ee-label { min-width: 0; }
      .ee-whatif .ee-search { max-width: 24rem; flex: 1 1 16rem; }
      .ee-whatif .ee-search input { padding-top: 0.3rem; padding-bottom: 0.3rem; font-size: 0.88rem; }
      .ee-button { border: 1px solid ${style.ink.axis}; background: #fff; border-radius: 0.4rem; padding: 0.2rem 0.6rem;
        font: inherit; font-size: 0.85rem; cursor: pointer; }
      .ee-note { color: ${style.ink.muted}; font-size: 0.85rem; }
    `;
    document.head.append(css);
  }

  const make = (tag, className, text) => {
    const node = document.createElement(tag);
    if (className) node.className = className;
    if (text !== undefined) node.textContent = text;
    return node;
  };

  // Search box with a list of matches; arrow keys move, Enter chooses.
  function searchBox(placeholder, kinds, onChoose) {
    const box = make("div", "ee-search");
    const input = make("input");
    input.type = "search";
    input.placeholder = placeholder;
    input.setAttribute("aria-label", placeholder);
    input.setAttribute("autocomplete", "off");
    const list = make("div", "ee-results");
    list.setAttribute("role", "listbox");
    list.hidden = true;
    box.append(input, list);
    let matches = [];
    let active = 0;
    const choose = unit => {
      list.hidden = true;
      input.value = "";
      input.blur();
      onChoose(unit);
    };
    const show = () => {
      matches = search(input.value, kinds);
      active = 0;
      list.replaceChildren(...(matches.length ? matches.map((unit, k) => {
        const item = make("div", "ee-result");
        item.setAttribute("role", "option");
        item.setAttribute("aria-selected", String(k === active));
        item.append(make("span", "", unit.label), make("small", "", unit.detail));
        // mousedown, so that the choice comes before the input loses focus
        item.addEventListener("mousedown", event => { event.preventDefault(); choose(unit); });
        return item;
      }) : [make("div", "ee-empty", "nothing found")]));
      list.hidden = !input.value.trim();
    };
    const highlight = () => [...list.children].forEach((item, k) =>
      item.setAttribute("aria-selected", String(k === active)));
    input.addEventListener("input", show);
    input.addEventListener("focus", () => { if (input.value.trim()) show(); });
    input.addEventListener("blur", () => { list.hidden = true; });
    input.addEventListener("keydown", event => {
      if (list.hidden || !matches.length) return;
      if (event.key === "ArrowDown") { active = (active + 1) % matches.length; highlight(); event.preventDefault(); }
      if (event.key === "ArrowUp") { active = (active - 1 + matches.length) % matches.length; highlight(); event.preventDefault(); }
      if (event.key === "Enter") { choose(matches[active]); event.preventDefault(); }
      if (event.key === "Escape") { list.hidden = true; }
    });
    return { box, input };
  }

  // Buttons of which one is pressed.
  function segmented(options, current, onChange) {
    const group = make("div", "ee-segmented");
    group.setAttribute("role", "group");
    options.forEach(option => {
      const button = make("button", "", option.label);
      button.type = "button";
      button.setAttribute("aria-pressed", String(option.value === current));
      button.addEventListener("click", () => onChange(option.value));
      group.append(button);
    });
    return group;
  }

  function choose(unit) {
    state.unit = { kind: unit.kind, index: unit.index };
    state.teacher = null;
    update();
  }

  const panel = make("div", "ee");
  const unitSearch = searchBox(
    "Search a school, place, municipality, province, or school board",
    ["national", "group", "school"], choose
  );
  const path = make("nav", "ee-path");
  path.setAttribute("aria-label", "chosen unit and the groups it belongs to");
  const card = make("div", "ee-card");
  const yearRow = make("div", "ee-row");
  const adviceRow = make("div", "ee-row");
  const whatIf = make("div", "ee-whatif");
  panel.append(unitSearch.box, path, card, yearRow, adviceRow, whatIf);
  el.before(panel);

  const teacherSearch = searchBox("type another school", ["school"], unit => {
    state.teacher = unit.index;
    update();
  });

  // the chosen unit and the groups it belongs to, as links
  function drawPath(v) {
    const steps = [{ label: "the Netherlands", unit: { kind: "national" } }];
    const group = (level, name) => {
      const index = groupIndex.get(`${level}|${name}`);
      if (index !== undefined) {
        steps.push({
          label: level === "similar" ? `similar schools (${name})` : name,
          unit: { kind: "group", index }
        });
      }
    };
    if (v.kind === "group") group(v.group.level, v.group.name);
    if (v.kind === "school") {
      const s = v.school;
      ["province", "municipality", "board", "similar"].forEach(level => group(level, s[level]));
      steps.push({ label: s.name, unit: state.unit });
    }
    path.replaceChildren(...steps.flatMap((step, k) => {
      const last = k === steps.length - 1;
      const node = make(last ? "span" : "button", last ? "ee-current" : "", step.label);
      if (!last) {
        node.type = "button";
        node.addEventListener("click", () => choose(step.unit));
      }
      return k === 0 ? [node] : [make("span", "ee-separator", "›"), node];
    }));
  }

  function drawCard(v) {
    const lines = [];
    if (v.kind === "national") {
      lines.push(["ee-name", "The Netherlands"],
        ["ee-detail", `${number.format(v.pupils)} pupils with a provisional advice and a test result in ${state.year}, from the CvTE tables`]);
    } else if (v.kind === "group") {
      const g = v.group;
      lines.push(["ee-name", groupLabel(g)],
        ["ee-detail", `${levelName[g.level]} · ${g.schools} schools · ${number.format(g.pupils)} pupils in ${schoolYear} · tests: ${g.tests}`]);
    } else {
      const s = v.school;
      lines.push(["ee-name", `${s.name}, ${s.place}`],
        ["ee-detail", `${s.municipality}, ${s.province} · board: ${s.board} · ${s.pupils} pupils in ${schoolYear} · test: ${s.provider}` +
          (s.weighting == null ? "" : ` · school weighting ${s.weighting.toFixed(1)}`)],
        ["ee-detail", `Teachers: ${verdict(s)}` + (s.pupils < 20 ? " (few pupils, so uncertain)" : "") + "."]);
    }
    card.replaceChildren(...lines.map(([className, text]) => make("div", className, text)));
  }

  function drawControls(v) {
    drawPath(v);
    drawCard(v);
    // year: only the Netherlands has a model for 2026
    yearRow.replaceChildren(make("span", "ee-label", "Test year"));
    if (v.kind === "national") {
      const years = national.pupils.map(d => d.test_year).sort();
      yearRow.append(segmented(years.map(year => ({ value: year, label: String(year) })), state.year,
        year => { state.year = year; state.test = ""; update(); }));
    } else {
      yearRow.append(make("span", "ee-note", `${schoolYear} (the only year with school data)`));
    }
    // the pupils: all, or those with one provisional advice
    adviceRow.replaceChildren(
      make("span", "ee-label", "Provisional advice"),
      segmented([{ value: "all", label: "all pupils" },
        ...labels.provisional.map((label, i) => ({ value: labels.provisional_keys[i], label }))],
      state.advice, advice => { state.advice = advice; update(); })
    );
    // what-ifs: another test, and for a school the teachers of another school
    const active = state.teacher != null || Boolean(state.test);
    whatIf.className = "ee-whatif" + (active ? " ee-active" : "");
    const testRow = make("div", "ee-row");
    testRow.append(
      make("span", "ee-label", v.kind === "school" ? "What if the school used" : "What if all pupils took"),
      segmented([{ value: "", label: v.kind === "school" ? "its own test" : "the tests they took" },
        ...v.tests.map(test => ({ value: test, label: test }))], state.test,
      test => { state.test = test; update(); })
    );
    const rows = [testRow];
    if (v.kind === "school") {
      const teacherRow = make("div", "ee-row");
      teacherRow.append(make("span", "ee-label", "What if it had the teachers of"));
      if (state.teacher != null) {
        const chosen = make("span", "", `${schools.name[state.teacher]} (${schools.place[state.teacher]})`);
        chosen.style.fontWeight = "600";
        const remove = make("button", "ee-button", "×");
        remove.type = "button";
        remove.setAttribute("aria-label", "back to its own teachers");
        remove.addEventListener("click", () => { state.teacher = null; update(); });
        teacherRow.append(chosen, remove);
      } else {
        teacherRow.append(teacherSearch.box);
      }
      rows.push(teacherRow);
    }
    if (active) {
      const reset = make("button", "ee-button", "Back to the real situation");
      reset.type = "button";
      reset.addEventListener("click", () => { state.teacher = null; state.test = ""; update(); });
      const resetRow = make("div", "ee-row");
      resetRow.append(reset);
      rows.push(resetRow);
    }
    whatIf.replaceChildren(...rows);
  }

  // ---------- figure ----------

  // shares as whole pupils out of 100 that add up to 100 (largest remainder)
  function perHundred(shares) {
    const raw = shares.map(p => 100 * p);
    const counts = raw.map(Math.floor);
    const left = 100 - counts.reduce((a, b) => a + b, 0);
    raw.map((value, k) => [value - Math.floor(value), k]).sort((a, b) => b[0] - a[0])
      .slice(0, left).forEach(([, k]) => { counts[k] += 1; });
    return counts;
  }

  // the three outcomes of the test (higher, the same, lower): the share of
  // pupils, a title, the test results behind the share, and what it means
  function outcomeTexts(results, v) {
    const advice = state.advice === "all" ? null : adviceLabel(state.advice);
    // the shares come from the model, not from the rounded icons, so that
    // they agree with the waterfall
    const total = test => Math.round(100 * results.filter(test).reduce((a, r) => a + r.share, 0));
    const possible = test => results.some(test);
    // the test results behind a share, each with the colour of its icons
    const dot = colour => `<span style='color:${colour}'>●</span>`;
    const breakdown = test => results.filter(r => Math.round(100 * r.share) > 0 && test(r))
      .map(r => `${dot(r.colour)} ${Math.round(100 * r.share)}% ${r.label}`).join("  ");
    const neighbour = gap => (results.find(r => r.gap === gap) || {}).label;
    const higher = r => r.gap > 0;
    const same = r => r.gap === 0;
    const lower = r => r.gap < 0;
    const raised = v.raised == null ? "" :
      ` In ${national.raised.year}, ${Math.round(100 * v.raised)}% of these advices were raised${v.raisedWhere}.`;
    return [
      possible(higher)
        ? [`${total(higher)}%`, "score higher", breakdown(higher), `The school must reconsider the advice.${raised}`]
        : ["–", "cannot score higher", "", `${advice} is the highest level.`],
      possible(same)
        ? [`${total(same)}%`, "score the same", "", "Teacher and test agree, so the advice stays."]
        : ["–", `cannot score exactly ${advice}`, "",
          `The test reports two levels at once, such as ${neighbour(-0.5)} or ${neighbour(0.5)}, so the result is always half a level lower or higher.`],
      possible(lower)
        ? [`${total(lower)}%`, "score lower", breakdown(lower), "Nothing changes: the test can only raise an advice, never lower it."]
        : ["–", "cannot score lower", "", `${advice} is the lowest level.`]
    ];
  }

  // text with an info icon that shows `info` on hover
  const info = `<span style='color:${style.ink.muted}'> ⓘ</span>`;
  const hoverlabel = { bgcolor: "#fff", bordercolor: style.ink.axis, font: { size: 12, color: style.ink.primary } };

  function draw(v) {
    const d = v.decomposition;
    const traces = el.data;
    const layout = el.layout;
    // waterfall
    const values = [d.category_design, d.teacher_error, d.leniency ?? 0, d.test_error, d.test_menu, d.eligible];
    const bases = [0];
    values.slice(0, 4).forEach(value => bases.push(bases[bases.length - 1] + value));
    bases.push(0);
    const isStep = [false, true, true, true, true, false];
    const notEstimated = k => k === 2 && d.leniency == null;
    Object.assign(traces[0], {
      x: values.map(value => 100 * value), base: bases.map(b => 100 * b), y: labels.rows,
      text: values.map((value, k) => (notEstimated(k) ? "–" : isStep[k] ? signed(value) : percent(value))),
      hovertext: values.map((value, k) => (notEstimated(k)
        ? `${labels.rows[k]}<br>not estimated for the Netherlands`
        : `${labels.rows[k]}<br>${(100 * Math.abs(value)).toFixed(1)} ${isStep[k] ? "percentage points" : "% of the pupils"}`))
    });
    const eligibleRow = labels.rows[labels.rows.length - 1];
    Object.assign(traces[1], {
      x: [v.observed == null ? null : 100 * v.observed], y: [eligibleRow], visible: v.observed != null,
      name: v.kind === "national" ? "observed (CvTE tables)" : "observed (DUO data)",
      hovertext: [`observed: ${(100 * (v.observed || 0)).toFixed(1)}% had a higher test advice (${v.kind === "national" ? "CvTE tables" : "DUO data"})`]
    });
    Object.assign(traces[2], {
      x: [v.asIs == null ? null : 100 * v.asIs], y: [eligibleRow], visible: v.asIs != null,
      hovertext: [`without the what-if: ${(100 * (v.asIs || 0)).toFixed(1)}% of the pupils`]
    });
    // 100 pupils (1% each), filled column by column from lower to higher
    const counts = perHundred(v.outcomes.map(r => r.share));
    const results = v.outcomes.map((r, k) => ({
      ...r, n: counts[k], colour: style.gaps[String(r.gap)], relation: labels.relations[String(r.gap)]
    }));
    const icons = [...results].sort((a, b) => a.gap - b.gap).flatMap(r => Array(r.n).fill(r));
    const iconTrace = traces[traces.length - 1];
    Object.assign(iconTrace, {
      x: icons.map((_, k) => Math.floor(k / 10) + 1),
      y: icons.map((_, k) => (k % 10) + 1),
      marker: { ...iconTrace.marker, color: icons.map(r => r.colour) },
      hovertext: icons.map(r => (state.advice === "all"
        ? `<b>${r.n}% of the pupils</b><br>test result ${r.relation}`
        : `<b>${r.label}</b><br>${r.n}% of the pupils<br>${r.relation}`))
    });
    // layout: headline, scale, connectors, and texts
    const ends = values.slice(0, 5).map((value, k) => bases[k] + value);
    layout.title.text = title(headline(v), data.subtitle);
    layout.xaxis.range = [0, 100 * v.top * 1.15];
    layout.shapes = ends.map((end, j) => ({
      type: "line", xref: "x", yref: "y", x0: 100 * end, x1: 100 * end,
      y0: j + 0.25, y1: j + 0.75, line: { color: style.ink.axis, width: 1 }
    }));
    const section = (text, y, note, hover) => ({
      x: 0, xref: "paper", xanchor: "left", xshift: 4 - style.left, y, yref: "paper",
      yanchor: "bottom", showarrow: false, align: "left",
      text: `<b>${text}</b>` + (note ? `  <span style='color:${style.ink.muted}'>${note}</span>` : "") +
        (hover ? info : ""),
      font: { size: 13, color: style.ink.primary },
      ...(hover ? { hovertext: hover, hoverlabel } : {})
    });
    const dots = [style.gaps["1"], style.gaps["0"], style.gaps["-1"]];
    const why = wrap(
      "The test reports two levels at once, such as havo/vwo, and such a result counts as higher than a havo advice. " +
      `So even with a perfect teacher and a perfect test, ${percent(d.category_design)} of these pupils would be eligible. ` +
      "Teachers and tests also make small mistakes, and some tests are a bit stricter than others. " +
      "A higher result is therefore no proof that the teacher underestimated a child.", 52);
    layout.annotations = [
      section("Where eligibility comes from", 0.97),
      // the names of the sources, with what they mean on hover
      ...labels.rows.map((name, k) => ({
        x: 0, xref: "paper", xanchor: "right", xshift: -6, y: name, yref: "y",
        showarrow: false, text: `<b>${name}</b>${info}`,
        font: { size: 12, color: style.ink.primary },
        hovertext: labels.info[k], hoverlabel
      })),
      section("Test results of these pupils", 0.42,
        v.estimated ? "estimated" : "observed (CvTE)", why),
      ...outcomeTexts(results, v).map(([share, name, breakdown, meaning], k) => ({
        x: 0.42, xref: "paper", xanchor: "left",
        y: [0.38, 0.27, 0.16][k], yref: "paper", yanchor: "top", showarrow: false, align: "left",
        text: `<span style='font-size:14px;color:${dots[k]}'>●</span> <span style='font-size:20px'><b>${share}</b></span> <b>${name}</b>${info}` +
          (breakdown ? `<br><span style='font-size:11px;color:${style.ink.secondary}'>${breakdown}</span>` : ""),
        font: { size: 12, color: style.ink.primary },
        hovertext: wrap(meaning, 52), hoverlabel
      }))
    ];
    Plotly.react(el, traces, layout);
  }

  // Computing a group from its schools takes up to about a second, so the
  // figure fades until it is done.
  function update() {
    const unit = state.unit;
    const busy = unit.kind === "group" && (Boolean(state.test) || !ownCells.has(unit.index));
    el.style.opacity = busy ? 0.5 : 1;
    setTimeout(() => {
      const v = view();
      drawControls(v);
      draw(v);
      el.style.opacity = 1;
    }, busy ? 20 : 0);
  }

  update();
}
