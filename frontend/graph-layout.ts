import {
  ISerializers,
  WidgetModel,
  WidgetView,
} from '@jupyter-widgets/base';

import { MODULE_NAME, MODULE_VERSION } from './version';

import cytoscape from 'cytoscape';

// @ts-ignore
import cola from 'cytoscape-cola';
cytoscape.use(cola);

// @ts-ignore
import cose_bilkent from 'cytoscape-cose-bilkent';
cytoscape.use( cose_bilkent );

// @ts-ignore
import dagre from 'cytoscape-dagre';
cytoscape.use(dagre);

// @ts-ignore
import euler from 'cytoscape-euler';
cytoscape.use(euler);

// @ts-ignore
import klay from 'cytoscape-klay';
cytoscape.use(klay);


export class GraphLayoutModel extends WidgetModel {
  defaults() {
    return {
      selector: "*",
      ...super.defaults()
    };
  }

  static serializers: ISerializers = {
    ...WidgetModel.serializers,
  };

  static model_name = 'GraphLayoutModel';
  static model_module = MODULE_NAME;
  static model_module_version = MODULE_VERSION;
  static view_name = 'GraphLayoutView';
  static view_module = MODULE_NAME;
  static view_module_version = MODULE_VERSION;
}


export class GraphLayoutView extends WidgetView {
  cytoscape_obj: any;

  constructor(params: any) {
    super({
      model: params.model,
      options: params.options,
    });

    this.cytoscape_obj = this.options.cytoscape_obj;

    this.model.on('change', this.render.bind(this), this);
  }

  render() {
    if (!this.cytoscape_obj) {
      this.el.innerHTML = "Cytoscape graph layout only usable in a Cytoscape widget.";
    } else {
      this.cytoscape_obj
        .filter(this.model.get('selector'))
        .layout(this.get_options())
        .run();
    }
  }

  get_options() {
    return {}
  }
}


export class NullLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      ...super.defaults()
    };
  }

  static serializers: ISerializers = {
    ...GraphLayoutModel.serializers,
  };

  static model_name = 'NullLayoutModel';
  static view_name = 'NullLayoutView';
}


export class NullLayoutView extends GraphLayoutView {
  get_options() {
    return {
      name: 'null'
    }
  }
}


export class RandomLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      bounding_box: null,
      ...super.defaults()
    };
  }

  static serializers: ISerializers = {
    ...GraphLayoutModel.serializers,
  };

  static model_name = 'RandomLayoutModel';
  static view_name = 'RandomLayoutView';
}


export class RandomLayoutView extends GraphLayoutView {
  get_options() {
    return {
      name: 'random',
      boundingBox: this.model.get('bounding_box')
    }
  }
}


export class PresetLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      ...super.defaults()
    };
  }

  static serializers: ISerializers = {
    ...GraphLayoutModel.serializers,
  };

  static model_name = 'PresetLayoutModel';
  static view_name = 'PresetLayoutView';
}


export class PresetLayoutView extends GraphLayoutView {
  get_options() {
    return {
      name: 'preset'
    }
  }
}


export class GridLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      bounding_box: null,
      ...super.defaults()
    };
  }

  static serializers: ISerializers = {
    ...GraphLayoutModel.serializers,
  };

  static model_name = 'GridLayoutModel';
  static view_name = 'GridLayoutView';
}


export class GridLayoutView extends GraphLayoutView {
  get_options() {
    return {
      name: 'grid',
      boundingBox: this.model.get('bounding_box')
    }
  }
}


export class CircleLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      bounding_box: null,
      ...super.defaults()
    };
  }

  static serializers: ISerializers = {
    ...GraphLayoutModel.serializers,
  };

  static model_name = 'CircleLayoutModel';
  static view_name = 'CircleLayoutView';
}


export class CircleLayoutView extends GraphLayoutView {
  get_options() {
    return {
      name: 'circle',
      boundingBox: this.model.get('bounding_box')
    }
  }
}


export class ConcentricLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      bounding_box: null,
      ...super.defaults()
    };
  }

  static serializers: ISerializers = {
    ...GraphLayoutModel.serializers,
  };

  static model_name = 'ConcentricLayoutModel';
  static view_name = 'ConcentricLayoutView';
}


export class ConcentricLayoutView extends GraphLayoutView {
  get_options() {
    return {
      name: 'concentric',
      boundingBox: this.model.get('bounding_box')
    }
  }
}


export class BreadthFirstLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      bounding_box: null,
      ...super.defaults()
    };
  }

  static serializers: ISerializers = {
    ...GraphLayoutModel.serializers,
  };

  static model_name = 'BreadthFirstLayoutModel';
  static view_name = 'BreadthFirstLayoutView';
}


export class BreadthFirstLayoutView extends GraphLayoutView {
  get_options() {
    return {
      name: 'breadthfirst',
      boundingBox: this.model.get('bounding_box')
    }
  }
}


export class CoseLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      bounding_box: null,
      ...super.defaults()
    };
  }

  static serializers: ISerializers = {
    ...GraphLayoutModel.serializers,
  };

  static model_name = 'CoseLayoutModel';
  static view_name = 'CoseLayoutView';
}


export class CoseLayoutView extends GraphLayoutView {
  get_options() {
    return {
      name: 'cose',
      boundingBox: this.model.get('bounding_box') || { x1: 0, y1: 0, w: this.cytoscape_obj.width(), h: this.cytoscape_obj.height() }
    }
  }
}


export class ColaLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      bounding_box: null,
      ...super.defaults()
    };
  }

  static serializers: ISerializers = {
    ...GraphLayoutModel.serializers,
  };

  static model_name = 'ColaLayoutModel';
  static view_name = 'ColaLayoutView';
}


export class ColaLayoutView extends GraphLayoutView {
  get_options() {
    return {
      name: 'cola',
      boundingBox: this.model.get('bounding_box') || { x1: 0, y1: 0, w: this.cytoscape_obj.width(), h: this.cytoscape_obj.height() }
    }
  }
}

