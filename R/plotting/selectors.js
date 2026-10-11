// Radio-button selectors above a plotly widget: each trace belongs to one
// combination of selector values, and only the traces of the chosen
// combination are visible; a trace without a value for a selector (null) is
// visible for every value. Later selectors only offer the values that exist
// for the earlier choices (e.g., the tests of the chosen year). Layout
// updates per combination (e.g., a title or annotations that depend on the
// choice) are applied with Plotly.relayout().
// Attached with htmlwidgets::onRender() by with_selectors() in
// R/plotting/selectors.R.
function (el, x, data) {
  // selectors: [{column, label}]; keys: one row of selector values per trace
  const { selectors, keys, initial } = data;
  const layouts = data.layouts || [];
  const state = { ...initial };
  const same = (a, b) => String(a) === String(b);
  const missing = value => value === null || value === undefined;
  const fits = (key, column) =>
    missing(key[column]) || same(key[column], state[column]);

  const wrapper = document.createElement("div");
  wrapper.style.cssText = "margin: 0.5em 0; text-align: left";
  const groups = selectors.map(({ column, label }) => {
    const row = document.createElement("div");
    row.innerHTML = `<span style="font-weight: 600; margin-right: 0.5em">${label}</span>`;
    const options = document.createElement("span");
    row.append(options);
    wrapper.append(row);
    return { column, options };
  });
  el.before(wrapper);

  function radio(column, value) {
    const option = document.createElement("label");
    option.style.cssText = "margin-right: 0.9em; white-space: nowrap";
    option.innerHTML =
      `<input type="radio" name="${el.id}-${column}"> ${value}`;
    const input = option.querySelector("input");
    input.checked = same(value, state[column]);
    input.addEventListener("change", () => {
      state[column] = value;
      refresh();
    });
    return option;
  }

  function refresh() {
    groups.forEach(({ column, options }, k) => {
      const earlier = groups.slice(0, k);
      const values = [...new Set(keys
        .filter(key => earlier.every(e => fits(key, e.column)))
        .map(key => key[column])
        .filter(value => !missing(value)))];
      // keep the choice if possible, else go back to the initial one
      if (!values.some(value => same(value, state[column]))) {
        state[column] = values.some(value => same(value, initial[column]))
          ? initial[column]
          : values[0];
      }
      options.replaceChildren(...values.map(value => radio(column, value)));
    });
    const visible = keys.map(key =>
      groups.every(({ column }) => fits(key, column))
    );
    Plotly.restyle(el, { visible });
    const layout = layouts.find(({ key }) =>
      groups.every(({ column }) => fits(key, column))
    );
    if (layout) Plotly.relayout(el, layout.patch);
  }

  refresh();
}
