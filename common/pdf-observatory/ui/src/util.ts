
declare global {
  interface RegExpConstructor {
    escape(s: string): string;
  }
}
RegExp.escape = function(s: string) {
    return s.replace(/[-/\\^$*+?.()|[\]{}]/g, '\\$&');
};

export {};

