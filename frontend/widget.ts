import {
  DOMWidgetModel,
  DOMWidgetView,
  ISerializers,
  WidgetModel,
  WidgetView,
} from '@jupyter-widgets/base';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const widgets = require('@jupyter-widgets/base');

import { MODULE_NAME, MODULE_VERSION } from './version';

// Import the CSS
import '../css/widget.css';

import cytoscape from 'cytoscape';
// @ts-ignore
import cola from 'cytoscape-cola';
// @ts-ignore
import popper from 'cytoscape-popper';
// @ts-ignore
import Tippy, { Instance } from 'tippy.js';
// @ts-ignore
import dagre from 'cytoscape-dagre';
// @ts-ignore
import cxtmenu from 'cytoscape-cxtmenu';

import 'tippy.js/themes/material.css';

cytoscape.use(popper);
cytoscape.use(dagre);
cytoscape.use(cola);
cytoscape.use(cxtmenu);


export class MenuCommandModel extends WidgetModel {
  defaults() {
    return {
      ...super.defaults(),
      fill_color: null,
      content: 'a command name',
      content_style: {},
      enabled: true,
    };
  }

  static model_name = 'MenuCommandModel';
  static model_module = MODULE_NAME;
  static model_module_version = MODULE_VERSION;
  static view_name = 'MenuCommandView';
  static view_module = MODULE_NAME;
  static view_module_version = MODULE_VERSION;
}

export class MenuCommandView extends WidgetView {
  constructor(params: any) {
    super({
      model: params.model,
      options: params.options,
    });
  }
}


export class ContextMenuModel extends WidgetModel {
  defaults() {
    return {
      ...super.defaults(),
      menu_radius: 100,
      selector: 'node',
      commands: [],
      fill_color: 'rgba(0, 0, 0, 0.75)',
      active_fill_color: 'rgba(1, 105, 217, 0.75)',
      active_padding: 20,
      indicator_size: 24,
      separator_width: 3,
      spotlight_padding: 4,
      min_spotlight_radius: 24,
      max_spotlight_radius: 38,
      open_menu_events: 'cxttapstart taphold',
      item_color: 'white',
      item_text_shadow_color: 'transparent',
      z_index: 9999,
      at_mouse: false,
    };
  }

  static serializers: ISerializers = {
    commands: { deserialize: widgets.unpack_models },
    ...WidgetModel.serializers,
  };

  static model_name = 'ContextMenuModel';
  static model_module = MODULE_NAME;
  static model_module_version = MODULE_VERSION;
  static view_name = 'ContextMenuView';
  static view_module = MODULE_NAME;
  static view_module_version = MODULE_VERSION;
}


export class ContextMenuView extends WidgetView {
  cytoscape_obj: any;
  is_rendered = false;
  menu: any = null;

  constructor(params: any) {
    super({
      model: params.model,
      options: params.options,
    });
    this.cytoscape_obj = this.options.cytoscape_obj;
  }

  async getCommands() {
    let commands: any = this.model.get('commands');

    return commands
      ? commands.map((command: any) => {
          return {
            fillColor: command.get('fill_color'),
            content: command.get('content'),
            contentStyle: command.get('content_style'),
            enabled: command.get('enabled'),
            select: (ele: any) => {
              command.send({ event: 'select', id: ele.data('id') });
            },
          };
        })
      : [];
  }

  render() {
    if (!this.is_rendered) {
      this.is_rendered = true;
      this.menu = this.cytoscape_obj.cxtmenu({
        menuRadius: this.model.get('menu_radius'),
        selector: this.model.get('selector'),
        commands: this.getCommands.bind(this),
        fillColor: this.model.get('fill_color'),
        activeFillColor: this.model.get('active_fill_color'),
        activePadding: this.model.get('active_padding'),
        indicatorSize: this.model.get('indicator_size'),
        separatorWidth: this.model.get('separator_width'),
        spotlightPadding: this.model.get('spotlight_padding'),
        minSpotlightRadius: this.model.get('min_spotlight_radius'),
        maxSpotlightRadius: this.model.get('max_spotlight_radius'),
        openMenuEvents: this.model.get('open_menu_events'),
        itemColor: this.model.get('item_color'),
        itemTextShadowColor: this.model.get('item_text_shadow_color'),
        zIndex: this.model.get('z_index'),
        atMouse: this.model.get('at_mouse'),
      });
    }
  }
}


export class ElementModel extends WidgetModel {
  defaults() {
    return {
      ...super.defaults(),
      _model_name: ElementModel.model_name,
      _model_module: ElementModel.model_module,
      _model_module_version: ElementModel.model_module_version,
      _view_name: ElementModel.view_name,
      _view_module: ElementModel.view_module,
      _view_module_version: ElementModel.view_module_version,

      group: '',
      removed: false,
      selected: false,
      selectable: true,
      locked: false,
      grabbed: false,
      grabbable: true,
      classes: [],
      data: {},
      position: {},
    };
  }

  static model_name = 'ElementModel';
  static model_module = MODULE_NAME;
  static model_module_version = MODULE_VERSION;
  static view_name = 'ElementView';
  static view_module = MODULE_NAME;
  static view_module_version = MODULE_VERSION;
}

export class CytoscapeModel extends DOMWidgetModel {
  defaults() {
    return {
      ...super.defaults(),
      _model_name: CytoscapeModel.model_name,
      _model_module: CytoscapeModel.model_module,
      _model_module_version: CytoscapeModel.model_module_version,
      _view_name: CytoscapeModel.view_name,
      _view_module: CytoscapeModel.view_module,
      _view_module_version: CytoscapeModel.view_module_version,

      elements: [],
      graph_style: [],
      graph_layout: {},

      zoom: 1,
      pan: { x: 0, y: 0 },

      min_zoom: 1e-50,
      max_zoom: 1e50,
      zooming_enabled: true,
      user_zooming_enabled: true,
      panning_enabled: true,
      user_panning_enabled: true,
      box_selection_enabled: false,
      selection_type: 'single',
      touch_tap_threshold: 8,
      desktop_tap_threshhold: 4,
      autolock: false,
      auto_ungrabify: false,
      auto_unselectify: false,

      headless: false,
      style_enabled: true,
      hide_edges_on_viewport: false,
      texture_on_viewport: false,
      motion_blur: false,
      motion_blur_opacity: 0.2,
      wheel_sensitivity: 1,
      pixel_ratio: 'auto',

      context_menus: [],

      tooltip_source: '',
    };
  }

  static serializers: ISerializers = {
    context_menus: { deserialize: widgets.unpack_models },
    elements: { deserialize: widgets.unpack_models },
    ...WidgetModel.serializers,
  };

  static model_name = 'CytoscapeModel';
  static model_module = MODULE_NAME;
  static model_module_version = MODULE_VERSION;
  static view_name = 'CytoscapeView';
  static view_module = MODULE_NAME;
  static view_module_version = MODULE_VERSION;
}

export class ElementView extends WidgetView {
  cytoscape_obj: any;
  is_rendered = false;

  constructor(params: any) {
    super({
      model: params.model,
      options: params.options,
    });
    this.cytoscape_obj = this.options.cytoscape_obj;

    this.model.on('change:selected', this.selected_changed, this);
    this.model.on('change:selectable', this.selectable_changed, this);
    this.model.on('change:removed', this.removed_changed, this);
    this.model.on('change:locked', this.locked_changed, this);
    this.model.on('change:grabbable', this.grabbable_changed, this);
    this.model.on('change:classes', this.classes_changed, this);
    this.model.on('change:data', this.data_changed, this);
    this.model.on('change:position', this.position_changed, this);
  }

  getElements() {
    return this.cytoscape_obj.filter((ele: any) => ele.data('_cid') == this.cid);
  }

  selected_changed() {
    if (this.model.get('selected')) {
      this.getElements().select();
    } else {
      this.getElements().unselect();
    }
  }

  selectable_changed() {
    if (this.model.get('selectable')) {
      this.getElements().selectify();
    } else {
      this.getElements().unselectify();
    }
  }

  removed_changed() {
    if (this.model.get('removed')) {
      this.getElements().remove();
    } else {
      this.getElements().restore();
    }
  }

  locked_changed() {
    if (this.model.get('locked')) {
      this.getElements().lock();
    } else {
      this.getElements().unlock();
    }
  }

  grabbable_changed() {
    if (this.model.get('grabbable')) {
      this.getElements().grabify();
    } else {
      this.getElements().ungrabify();
    }
  }

  classes_changed() {
    this.getElements().classes(this.model.get('classes'));
  }

  data_changed() {
    this.getElements().data(Object.assign({ _cid: this.cid }, this.model.get('data')));
  }

  position_changed() {
    this.getElements().position(this.model.get('position'));
  }

  render() {
    if (!this.is_rendered) {
      this.is_rendered = true;

      this.cytoscape_obj.add({
        group: this.model.get('group'),
        data: Object.assign({ _cid: this.cid }, this.model.get('data')),
        classes: this.model.get('classes'),
        selectable: this.model.get('selectable'),
        selected: this.model.get('selected'),
      });
    }
  }
}

export class CytoscapeView extends DOMWidgetView {
  cytoscape_obj: any;
  layout: any;
  is_rendered = false;
  elementViews: any;
  contextMenuViews: any;

  constructor(options?: Backbone.ViewOptions<WidgetModel> & { options?: any }) {
    super(options || {});
    this.elementViews = new widgets.ViewList(
      this.createElementView,
      this.removeElementView,
      this
    );
    this.contextMenuViews = new widgets.ViewList(
      this.createContextMenuView,
      this.removeContextMenuView,
      this
    );
  }

  async findElementView(cid: string) {
    let views: Array<ElementView> = await Promise.all(this.elementViews.views);

    for (let view of views) {
      if (view.cid == cid) return view;
    }
  }

  handleEvent(event: Event): void {
    if (event.type === 'contextmenu') event.stopPropagation();
  }

  render() {
    if (!this.is_rendered) {
      this.is_rendered = true;
      this.displayed.then(() => {
        this.el.classList.add('cytoscape-widget');
        this.el.addEventListener('contextmenu', this);

        this.cytoscape_obj = cytoscape({
          container: this.el,
          minZoom: this.model.get('min_zoom'),
          maxZoom: this.model.get('max_zoom'),
          zoomingEnabled: this.model.get('zooming_enabled'),
          userZoomingEnabled: this.model.get('user_zooming_enabled'),
          panningEnabled: this.model.get('panning_enabled'),
          boxSelectionEnabled: this.model.get('box_selection_enabled'),
          selectionType: this.model.get('selection_type'),
          touchTapThreshold: this.model.get('touch_tap_threshold'),
          desktopTapThreshold: this.model.get('desktop_tap_threshold'),
          autolock: this.model.get('autolock'),
          autoungrabify: this.model.get('auto_ungrabify'),
          autounselectify: this.model.get('auto_unselectify'),
          headless: this.model.get('headless'),
          styleEnabled: this.model.get('style_enabled'),
          hideEdgesOnViewport: this.model.get('hide_edges_on_viewport'),
          textureOnViewport: this.model.get('texture_on_viewport'),
          motionBlur: this.model.get('motion_blur'),
          motionBlurOpacity: this.model.get('motion_blur_opacity'),
          wheelSensitivity: this.model.get('wheel_sensitivity'),
          style: this.model.get('graph_style')
        });

        this.cytoscape_obj.on('add', async (e: any) => {
          const view = await this.findElementView(e.target.data('_cid'));

          if (view) {
            view.model.set('removed', false);
            view.model.save_changes();
          }
        });

        this.cytoscape_obj.on('remove', async (e: any) => {
          const view = await this.findElementView(e.target.data('_cid'));

          if (view) {
            view.model.set('removed', true);
            view.model.save_changes();
          }
        });

        this.cytoscape_obj.on('select', async (e: any) => {
          const view = await this.findElementView(e.target.data('_cid'));

          if (view) {
            view.model.set('selected', true);
            view.model.save_changes();
          }
        });

        this.cytoscape_obj.on('unselect', async (e: any) => {
          const view = await this.findElementView(e.target.data('_cid'));

          if (view) {
            view.model.set('selected', false);
            view.model.save_changes();
          }
        });

        this.cytoscape_obj.on('lock', async (e: any) => {
          const view = await this.findElementView(e.target.data('_cid'));

          if (view) {
            view.model.set('locked', true);
            view.model.save_changes();
          }
        });

        this.cytoscape_obj.on('unlock', async (e: any) => {
          const view = await this.findElementView(e.target.data('_cid'));

          if (view) {
            view.model.set('locked', false);
            view.model.save_changes();
          }
        });

        this.cytoscape_obj.on('grab', async (e: any) => {
          const view = await this.findElementView(e.target.data('_cid'));

          if (view) {
            view.model.set('grabbed', true);
            view.model.save_changes();
          }
        });

        this.cytoscape_obj.on('free', async (e: any) => {
          const view = await this.findElementView(e.target.data('_cid'));

          if (view) {
            view.model.set('grabbed', false);
            view.model.save_changes();
          }
        });

        this.cytoscape_obj.on('position', async (e: any) => {
          const view = await this.findElementView(e.target.data('_cid'));

          if (view) {
            view.model.set('position', e.target.position());
            view.model.save_changes();
          }
        });

        this.cytoscape_obj.on('click', (e: any) => {
          const element = e.target;
          const ref = element.popperRef();
          const dummyDomEle = document.createElement('div');

          const tooltip_source = this.model.get('tooltip_source');
          if (element.data()[tooltip_source]) {
            const tip = Tippy(dummyDomEle, {
              trigger: 'manual',
              lazy: false,
              arrow: true,
              theme: 'material',
              placement: 'bottom',
              content: () => {
                const content = document.createElement('div');
                content.innerHTML = element
                  .data()
                  [tooltip_source].replace(/(?:\r\n|\r|\n)/g, '<br>');
                return content;
              },
              onCreate: (instance: Instance | undefined) => {
                if (instance && instance.popperInstance) {
                  instance.popperInstance.reference = ref;
                }
              },
            });
            tip.show();
          }
        });

        this.model.on('change:elements', this.elements_changed, this);

        this.model.on('change:context_menus', this.context_menus_changed, this);

        this.model.on('change:min_zoom', () => {
            this.cytoscape_obj.minZoom(this.model.get('min_zoom'));
          }, this);

        this.model.on('change:max_zoom', () => {
            this.cytoscape_obj.maxZoom(this.model.get('max_zoom'));
          }, this);


        this.model.on('change:zooming_enabled', () => {
            this.cytoscape_obj.zoomingEnabled(this.model.get('zooming_enabled'));
          }, this);

        this.model.on('change:user_zooming_enabled', () => {
            this.cytoscape_obj.userZoomingEnabled(this.model.get('user_zooming_enabled'));
          }, this);

        this.model.on('change:panning_enabled', () => {
            this.cytoscape_obj.selectionType(this.model.get('panning_enabled'));
          }, this);

        this.model.on('change:box_selection_enabled', () => {
            this.cytoscape_obj.boxSelectionEnabled(this.model.get('box_selection_enabled'));
          }, this);

        this.model.on('change:selection_type', () => {
            this.cytoscape_obj.selectionType(this.model.get('selection_type'));
          }, this);

        this.model.on('change:autolock', () => {
            this.cytoscape_obj.autolock(this.model.get('autolock'));
          }, this);

        this.model.on('change:auto_ungrabify', () => {
            this.cytoscape_obj.autoungrabify(this.model.get('auto_ungrabify'));
          }, this);

        this.model.on('change:auto_unselectify', () => {
            this.cytoscape_obj.autounselectify(this.model.get('auto_unselectify'));
          }, this);

        this.model.on('change:graph_style', () => {
            this.cytoscape_obj.style(this.model.get('graph_style'));
          }, this);

        this.model.on('change:graph_layout', this.graph_layout_changed, this);

        this.cytoscape_obj.ready(() => {
          this.context_menus_changed();
          this.elements_changed();
        });
      });
    }
  }

  elements_changed() {
    this.elementViews.update(this.model.get('elements'))
      .then((views: Array<any>) => Promise.all(views.map((view: any) => view.render())))
      .then(() => this.graph_layout_changed());
  }

  context_menus_changed() {
    this.contextMenuViews.update(this.model.get('context_menus'))
      .then((views: Array<any>) => Promise.all(views.map((view: any) => view.render())));
  }

  graph_layout_changed() {
    let layout = this.model.get('graph_layout');
    this.layout = layout ? this.cytoscape_obj.layout(layout) : null;
    if (this.layout) this.layout.run();
  }

  createElementView(model: any, index: any) {
    return this.create_child_view(model, {
      cytoscape_obj: this.cytoscape_obj
    });
  }

  removeElementView(view: any) {
    view.remove();
  }

  createContextMenuView(model: any, index: any) {
    return this.create_child_view(model, {
      cytoscape_obj: this.cytoscape_obj
    });
  }

  removeContextMenuView(view: any) {
    view.remove();
  }
}

