import {
  DOMWidgetModel,
  DOMWidgetView,
  ISerializers,
  StyleModel,
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
import popper from 'cytoscape-popper';
// @ts-ignore
import Tippy, { Instance } from 'tippy.js';

import 'tippy.js/themes/material.css';

cytoscape.use(popper);


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

      group: 'nodes',
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

export class CytoscapeStyleModel extends StyleModel {
  defaults() {
    return {
      ...super.defaults(),
      _model_name: CytoscapeStyleModel.model_name,
      _model_module: CytoscapeStyleModel.model_module,
      _model_module_version: CytoscapeStyleModel.model_module_version
    };
  }

  static model_name = 'CytoscapeStyleModel';
  static model_module = MODULE_NAME;
  static model_module_version = MODULE_VERSION;
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
      graph_layouts: [],

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
    graph_layouts: { deserialize: widgets.unpack_models },
    ...DOMWidgetModel.serializers,
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

    this.on('remove', () => {
      if (this.cytoscape_obj) {
        this.getElements().remove();
      }
    });
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
    if (this.cytoscape_obj) this.cytoscape_obj.graph_layouts_changed();
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
    this.getElements().data({ _cid: this.cid, ...this.model.get('data') });
  }

  position_changed() {
    this.getElements().position(this.model.get('position'));
  }

  render() {
    if (!this.cytoscape_obj) {
      this.el.innerHTML = "Cytoscape element only usable in a Cytoscape widget.";
    } else if (!this.is_rendered) {
      this.is_rendered = true;

      let ele = this.cytoscape_obj.add({
        group: this.model.get('group'),
        data: { _cid: this.cid, ...this.model.get('data') },
        classes: this.model.get('classes'),
        selectable: this.model.get('selectable'),
        selected: this.model.get('selected'),
      });

      if (this.model.get('removed')) this.cytoscape_obj.remove(ele);
    }
  }
}

export class CytoscapeView extends DOMWidgetView {
  cytoscape_obj: any;
  is_rendered = false;
  elementViews: any;
  contextMenuViews: any;
  layoutViews: any;
  cy_container: any;

  initialize(parameters: any): void {
    super.initialize(parameters);
    this.elementViews = new widgets.ViewList(
      this.create_cy_child_view,
      this.remove_cy_child_view,
      this
    );
    this.contextMenuViews = new widgets.ViewList(
      this.create_cy_child_view,
      this.remove_cy_child_view,
      this
    );
    this.layoutViews = new widgets.ViewList(
      this.create_cy_child_view,
      this.remove_cy_child_view,
      this
    );
    this.model.on('msg:custom', this.handle_custom_message.bind(this));
  }

  async findElementView(cid: string) {
    let views: Array<ElementView> = await Promise.all(this.elementViews.views);

    for (let view of views) {
      if (view.cid == cid) return view;
    }
  }

  handle_custom_message(content: any): void {
    if (this.cytoscape_obj) {
      this.cytoscape_obj.ready(() => {
        switch (content.do) {
          case 'reset':
            this.cytoscape_obj.reset();
            break;
          case 'fit':
            this.cytoscape_obj.fit();
            break;
          case 'center':
            this.cytoscape_obj.center();
            break;
          case 'toggle_fullscreen':
            if (document.fullscreenElement) {
              document.exitFullscreen();
            } else {
              this.el.requestFullscreen();
            }
            break;
        }
      });
    }
  }

  handleEvent(event: Event): void {
    if (event.type === 'contextmenu') event.stopPropagation();
  }

  render() {
    super.render();
    this.displayed.then(() => {

      this.el.classList.add('cytoscape-widget');
      this.el.classList.add('jupyter-widgets');
      this.el.addEventListener('contextmenu', this);
      this.cy_container = document.createElement('div');
      this.el.appendChild(this.cy_container);

      this.cytoscape_obj = cytoscape({
        container: this.cy_container,
        minZoom: this.model.get('min_zoom'),
        maxZoom: this.model.get('max_zoom'),
        zoomingEnabled: this.model.get('zooming_enabled'),
        zoom: this.model.get('zoom'),
        pan: this.model.get('pan'),
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

      this.wireCytoscape();

      this.wireModel();
    });
  }

  wireModel() {
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

    this.model.on('change:zoom', () => {
        this.cytoscape_obj.zoom(this.model.get('zoom'));
      }, this);

    this.model.on('change:pan', () => {
        this.cytoscape_obj.pan(this.model.get('pan'));
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

    this.model.on('change:graph_layouts', this.graph_layouts_changed, this);
  }

  wireCytoscape() {
    this.cytoscape_obj.on('pan', (e: any) => {
      this.model.set('pan', this.cytoscape_obj.pan());
      this.model.save_changes();
    });

    this.cytoscape_obj.on('zoom', (e: any) => {
      this.model.set('zoom', this.cytoscape_obj.zoom());
      this.model.save_changes();
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

    this.cytoscape_obj.ready(() => {
      this.context_menus_changed();
      this.elements_changed();
    });
  }

  processPhosphorMessage(msg: any): void {
    super.processPhosphorMessage(msg);
    if ((msg.type === 'resize' || msg.type === 'after-show') && this.cytoscape_obj) {
      this.cytoscape_obj.resize();
      this.cytoscape_obj.fit();
    }
  }

  elements_changed() {
    this.elementViews.update(this.model.get('elements'))
      .then((views: Array<any>) => Promise.all(views.map((view: any) => view.render())))
      .then(() => this.graph_layouts_changed());
  }

  context_menus_changed() {
    this.contextMenuViews.update(this.model.get('context_menus'))
      .then((views: Array<any>) => Promise.all(views.map((view: any) => view.render())));
  }

  graph_layouts_changed() {
    this.layoutViews.update(this.model.get('graph_layouts'))
      .then((views: Array<any>) => Promise.all(views.map((view: any) => view.render())))
  }

  create_cy_child_view(model: any, index: any) {
    return this.create_child_view(model, {
      cytoscape_obj: this.cytoscape_obj
    });
  }

  remove_cy_child_view(view: any) {
    view.remove();
  }
}

