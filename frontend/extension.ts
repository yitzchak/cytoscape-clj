(window as any).__webpack_public_path__ =
  document.querySelector('body')!.getAttribute('data-base-url') + // eslint-disable-line @typescript-eslint/no-non-null-assertion
  'nbextensions/cl-cytoscape';

export * from './index';
