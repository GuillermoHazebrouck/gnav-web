const cacheName = "4C4";
const precachedResources = ["/",
                            "/index.html",
                            "/main.wasm",
                            "/adawebpack.mjs",
                            "/manifest.json",
                            "/bweep.wav",
                            "/atent.wav",
                            "/alert.wav",
                            "/traffic.bin",
                            "/metar.bin",
                            "/terrain_1.bin",
                            "/terrain_2.bin",
                            "/terrain_3.bin",
                            "/terrain_4.bin",
                            "/terrain_5.bin",
                            "/reference.bin",
                            "/layers.bin",
                            "/airspaces.bin",
                            "/aircraft.bin"];

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
