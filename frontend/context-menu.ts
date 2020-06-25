import {
  ISerializers,
  WidgetModel,
  WidgetView,
} from '@jupyter-widgets/base';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const widgets = require('@jupyter-widgets/base');

import { MODULE_NAME, MODULE_VERSION } from './version';

import cytoscape from 'cytoscape';

// @ts-ignore
import cxtmenu from 'cytoscape-cxtmenu';
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

  render () {
    this.el.innerHTML = "Cytoscape menu command only usable in a Cytoscape context menu.";
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

    this.on('remove', () => {
      if (this.menu) {
        this.menu.destroy();
        this.menu = null;
        this.is_rendered = false;
      }
    });
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
            select: (ele: any) => command.send({ event: 'select', id: ele.data('id') }),
          };
        })
      : [];
  }

  createMenu() {
    if (this.menu) this.menu.destroy();

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

  render() {
    if (!this.cytoscape_obj) {
      this.el.innerHTML = "Cytoscape context menu only usable in a Cytoscape widget.";
    } else if (!this.is_rendered) {
      this.is_rendered = true;

      this.createMenu();

      this.model.on_some_change([
        'menu_radius',
        'selector',
        'fill_color',
        'active_fill_color',
        'active_padding',
        'indicator_size',
        'separator_width',
        'spotlight_padding',
        'min_spotlight_radius',
        'max_spotlight_radius',
        'open_menu_events',
        'item_color',
        'item_text_shadow_color',
        'z_index',
        'at_mouse'
      ], this.createMenu.bind(this), this);
    }
  }
}


