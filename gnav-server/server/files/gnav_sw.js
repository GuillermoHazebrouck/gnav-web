// ** G-NAV service worker **
// Do not forget increasing version and cleaning proxy cache
// when adapting the resources!
// NOTE: added favicon.ico due to exception Firefox
//--------------------------------------------------------------
const cacheName = "5B";
const precachedResources = ["/",
                            "/favicon.ico",
                            "/index.html",
                            "/main.wasm",
                            "/adawebpack.mjs",
                            "/manifest.json",
                            "/terrain_1.bin",
                            "/terrain_2.bin",
                            "/terrain_3.bin",
                            "/terrain_4.bin",
                            "/terrain_5.bin",
                            "/reference.bin",
                            "/layers.bin",
                            "/airspaces.bin",
                            "/aircraft.bin",
                            "/bweep.wav",
                            "/atent.wav",
                            "/alert.wav",
                            "/score.wav",
                            "/messg.wav"];

async function precache() {
  const cache = await caches.open(cacheName);
  return cache.addAll(precachedResources);
}

self.addEventListener("install", (event) => {
  event.waitUntil(precache());
});

async function networkFirst(request) {
  try {
    const networkResponse = await fetch(request);
    if (networkResponse.ok) {
      const cache = await caches.open(cacheName);
      cache.put(request, networkResponse.clone());
    }
    return networkResponse;
  } catch (error) {
    const cachedResponse = await caches.match(request);
    return cachedResponse || Response.error();
  }
}

self.addEventListener("fetch", (event) => {
  const url = new URL(event.request.url);
  if ((url.pathname === "/traffic.bin") || (url.pathname === "/metar.bin")) {
    //event.respondWith(networkFirst(event.request));
    return;
  } else {
    event.respondWith(caches.match(event.request));
  }
});
