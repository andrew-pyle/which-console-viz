// Start listening on port 8080 of localhost.
const server = Deno.listen({ port: 8080 });
console.log(`HTTP webserver running.  Access it at:  http://localhost:8080/`);

const { API_AUTH: apiAuth, CLIENT_ID: clientId } = Deno.env.toObject();

// Connections to the server will be yielded up as an async iterable.
for await (const conn of server) {
  // In order to not be blocking, we need to handle each connection individually
  // without awaiting the function
  serveHttp(conn);
}

async function serveHttp(conn: Deno.Conn) {
  // This "upgrades" a network connection into an HTTP connection.
  const httpConn = Deno.serveHttp(conn);
  // Each request sent over the HTTP connection will be yielded as an async
  // iterator from the HTTP connection.
  for await (const requestEvent of httpConn) {
    //   // The native HTTP server uses the web standard `Request` and `Response`
    //   // objects.
    //   const body = `Your user-agent is:\n\n${requestEvent.request.headers.get(
    //     "user-agent",
    //   ) ?? "Unknown"}`;
    //   // The requestEvent's `.respondWith()` method is how we send the response
    //   // back to the client.
    //   requestEvent.respondWith(
    //     new Response(body, {
    //       status: 200,
    //     }),
    //   );

    // const endpointToProxy = new URL(requestEvent.request.url).pathname;
    // const endpoint = new URL(requestEvent.request.url).searchParams.get(
    //   "endpoint",
    // );
    // const body = await requestEvent.request.text();

    // const id = new URL(requestEvent.request.url).searchParams.get("id");

    // // Make request on behalf of proxy client
    // const igdbApiRestEndpoint = new URL(
    //   endpointToProxy,
    //   `https://api.igdb.com`,
    // );

    // const apiRes = await fetch(igdbApiRestEndpoint, {
    //   headers: {
    //     "Client-ID": clientId,
    //     "Authorization": apiAuth,
    //   },
    //   method: requestEvent.request.method,
    //   body: await requestEvent.request.text(), //requestEvent.request.body, //body, //`where id = ${id}; fields id,name;`,
    // });

    // // Return proxied IGDB API request to client with CORS header
    // const proxyRes = new Response(apiRes.body, {
    //   status: apiRes.status,
    //   statusText: apiRes.statusText,
    //   headers: {
    //     // ...Object.fromEntries(apiRes.headers.entries()),
    //     "Access-Control-Allow-Origin": "http://localhost:8000",
    //   },
    // });

    // requestEvent.respondWith(proxyRes);

    // Create the proxied URL
    const proxyEndpoint = new URL(
      new URL(requestEvent.request.url).pathname,
      `https://api.igdb.com`
    ).toString();

    // Add IGDB Authorization Headers
    const clientRequestHeaders = new Headers(requestEvent.request.headers);
    clientRequestHeaders.set("Authorization", apiAuth);
    clientRequestHeaders.set("Client-ID", clientId);

    const proxyRequest = new Request(proxyEndpoint, {
      headers: clientRequestHeaders,
      body: requestEvent.request.body,
      method: "POST",
    });

    // Proxy client's request
    const apiResponse = await fetch(proxyRequest);

    // Add CORS headers to response
    const proxyResponseHeaders = new Headers(apiResponse.headers);
    proxyResponseHeaders.set(
      "Access-Control-Allow-Origin",
      "http://localhost:8000"
    );
    const proxyResponse = new Response(apiResponse.body, {
      headers: proxyResponseHeaders,
    });

    // Respond to client
    requestEvent.respondWith(proxyResponse);
  }
}
