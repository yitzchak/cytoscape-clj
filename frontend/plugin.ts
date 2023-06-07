import { Application, IPlugin } from '@lumino/application';

import { Widget } from '@lumino/widgets';

import { IJupyterWidgetRegistry } from '@jupyter-widgets/base';

import { MODULE_NAME, MODULE_VERSION } from './version';

const EXTENSION_ID = 'cytoscape-clj:plugin';

const cytoscapePlugin: IPlugin<Application<Widget>, void> = {
  id: EXTENSION_ID,
  requires: [IJupyterWidgetRegistry as any],
  activate: activateWidgetExtension,
  autoStart: true,
};

export default cytoscapePlugin;

/**
 * Activate the widget extension.
 */
function activateWidgetExtension(
  app: Application<Widget>,
  registry: IJupyterWidgetRegistry
): void {
  registry.registerWidget({
    name: MODULE_NAME,
    version: MODULE_VERSION,

    exports: async () => {
      return {
        ...await import('./widget'),
        ...await import('./graph-layout'),
        ...await import('./context-menu')
      }
    }
  });
}


