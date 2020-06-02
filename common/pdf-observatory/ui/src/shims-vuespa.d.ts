import Vue from 'vue';
declare module 'vue' {
  export default interface Vue {
    $vuespa: {
      // Call a remote method, and return the result
      call: (fn: string, ...args: any[]) => Promise<any>,
      /** Set up a handler for HTTP endpoints. First argument is a function
        called whenever the websocket's identity changes. The second argument is
        a list of available handlers, each of which take one argument, which holds
        the query parameters given to the HTTP request.

        Returns: a function which, when called, unbinds the handler. Often,
        this belongs in Vue's ``beforeDestroy`` callback.
        */
      httpHandler: (cb: {(url: string): void}, fns: {[name: string]: {(args: any): void}}) => {(): void},
      // Call a remote method, and update `name` on this local Vue instance.
      update: (name: string, fn: string, ...args: any[]) => Promise<void>,
    };
  }
}

