import { description } from "./description";
import { bindScroll } from "./scroll";
import { fetchFactory } from "./fetch";
import { handleSearch } from "./search";

$(() => {
  window.Shiny.addCustomMessageHandler("blockr-registry-endpoints", (msg) => {
    const endpoints = fetchFactory(msg);
    setTimeout(() => {
      bindScroll(msg, endpoints);
      handleSearch(msg, endpoints);
      description();
    }, msg.delay);
  });
});
