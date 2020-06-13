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

import 'tippy.js/themes/material.css';

cytoscape.use(popper);
cytoscape.use(dagre);
cytoscape.use(cola);

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
      cytoscape_style: [],
      cytoscape_layout: {},
      rendered_position: {},

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

      tooltip_source: '',
    };
  }

  static serializers: ISerializers = {
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
  parentModel: any;

  constructor(params: any) {
    super({
      model: params.model,
      options: params.options,
    });
    this.parentModel = this.options.parentModel;

    this.model.on('change:group', this.groupChanged, this);
    this.model.on('change:removed', this.removedChanged, this);
    this.model.on('change:selected', this.selectedChanged, this);
    this.model.on('change:locked', this.lockedChanged, this);
    this.model.on('change:grabbed', this.grabbedChanged, this);
    this.model.on('change:grabbable', this.grabbableChanged, this);
    this.model.on('change:classes', this.classesChanged, this);
    this.model.on('change:data', this.dataChanged, this);
    this.model.on('change:position', this.positionChanged, this);
  }

  //TODO: not sure if this is necessary to propagate the changes...
  groupChanged() {
    this.parentModel.set('group', this.model.get('group'));
  }

  removedChanged() {
    this.parentModel.set('removed', this.model.get('removed'));
  }

  selectedChanged() {
    this.parentModel.set('selected', this.model.get('selected'));
  }

  lockedChanged() {
    this.parentModel.set('locked', this.model.get('locked'));
  }

  grabbedChanged() {
    this.parentModel.set('grabbed', this.model.get('grabbed'));
  }

  grabbableChanged() {
    this.parentModel.set('grabbable', this.model.get('grabbable'));
  }

  classesChanged() {
    this.parentModel.set('classes', this.model.get('classes'));
  }

  dataChanged() {
    this.parentModel.set('data', this.model.get('data'));
  }

  positionChanged() {
    this.parentModel.set('position', this.model.get('position'));
  }
}

export class CytoscapeView extends DOMWidgetView {
  cytoscape_obj: any;
  is_rendered = false;
  elementViews: any = [];

  render() {
    this.el.classList.add('cytoscape-widget');

    this.elementViews = new widgets.ViewList(
      this.addElementModel,
      this.removeElementView,
      this
    );
    this.elementViews.update(this.model.get('elements'));

    this.value_changed();

    this.model.on('change:elements', this.value_changed, this);
    //TODO: not sure if these are useful, the one for style is not
    //but for layout it seems to make a difference. Need to test and
    //remove the ones that are not and figure out why
    // TODO 2:
    // some of these changes do not require re-running init_render
    // there are cytoscapejs functions that can be called to run change
    // these options
    this.model.on('change:min_zoom', this.value_changed, this);
    this.model.on('change:max_zoom', this.value_changed, this);
    this.model.on('change:zooming_enabled', this.value_changed, this);
    this.model.on('change:user_zooming_enabled', this.value_changed, this);
    this.model.on('change:panning_enabled', this.value_changed, this);
    this.model.on('change:box_selection_enabled', this.value_changed, this);
    this.model.on('change:selection_type', this.value_changed, this);
    this.model.on('change:touch_tap_threshold', this.value_changed, this);
    this.model.on('change:desktop_tap_threshold', this.value_changed, this);
    this.model.on('change:autolock', this.value_changed, this);
    this.model.on('change:auto_ungrabify', this.value_changed, this);
    this.model.on('change:auto_unselectify', this.value_changed, this);
    this.model.on('change:cytoscape_layout', this.value_changed, this);
    this.model.on('change:cytoscape_style', this.value_changed, this);
    this.model.on('change:elements', this.value_changed, this);

    this.displayed.then(() => {
      this.init_render();
    });
  }

  value_changed() {
    if (this.is_rendered) {
      console.log('value_changed');
      this.init_render();
    }
  }

  converts_dict() {
    const graph: Array<any> = [];

    for (let element of this.model.get('elements')) {
      graph.push({
        group: element.get('group'),
        data: Object.assign({ _model: element }, element.get('data')),
        classes: element.get('classes'),
        selectable: element.get('selectable'),
        selected: element.get('selected'),
      });
    }

    return graph;
  }

  init_render() {
    this.is_rendered = true;
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
      layout: this.model.get('cytoscape_layout'),
      style: this.model.get('cytoscape_style'),
      elements: this.converts_dict(),
    });

    this.cytoscape_obj.on('select', (e: any) => {
      const model = e.target.data()._model;

      model.set('selected', true);
      model.save_changes();
    });

    this.cytoscape_obj.on('unselect', (e: any) => {
      const model = e.target.data()._model;

      model.set('selected', false);
      model.save_changes();
    });

    this.cytoscape_obj.on('click', (e: any) => {
      const element = e.target;
      const ref = element.popperRef();
      const dummyDomEle = document.createElement('div');

      const tooltip_source = this.model.get('tooltip_source');
      if (element.data()[tooltip_source]) {
        const tip = Tippy(dummyDomEle, {
          //TODO: add a pretty tippy
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
  }

  addElementModel(ElementModel: any) {
    return this.create_child_view(ElementModel, {
      cytoscapeView: this,
      parentModel: this.model,
    });
  }

  removeElementView(view: any) {
    view.remove();
  }

}
