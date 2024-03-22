import { renderPills } from "./render";

const BATCH = 20;

export const unbindScroll = (params) => {
  $(`#${params.ns}-scrollable`).off("scroll");
};

export const bindScroll = (params, endpoints) => {
  fetchUntilScrollable(params, endpoints);

  unbindScroll(params);

  $(`#${params.ns}-scrollable`).on("scroll", () => {
    unbindScroll(params);
    const n = getNBlocks(params.ns);
    endpoints.fetchLeast().then((data) => {
      const to = n + BATCH;
      if (to > data.length) return;

      renderPills(params, data.slice(n, to));
      bindScroll(params);
    });
  });
};

async function fetchUntilScrollable(params, endpoints) {
  const n = getNBlocks(params.ns);

  return endpoints.fetchLeast().then((data) => {
    if (!data.length) return;

    const to = n + BATCH;
    if (to > data.length) return;

    renderPills(params, data.slice(n, to));
    if (
      $(`#${params.ns}-scrollable-child`).height() <=
      $(`#${params.ns}-scrollable`).height()
    ) {
      fetchUntilScrollable(params, endpoints);
    }
  });
}

const getNBlocks = (ns) => {
  return $(`#${ns}-scrollable`).find(".add-block").length;
};