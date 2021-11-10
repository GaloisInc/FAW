
export function regexEscape(s: string) {
  // Thanks https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions "Escaping" section
  return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

