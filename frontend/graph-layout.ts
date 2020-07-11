import {
  WidgetModel,
  WidgetView,
} from '@jupyter-widgets/base';

import { MODULE_NAME, MODULE_VERSION } from './version';

import cytoscape from 'cytoscape';

// @ts-ignore
import cola from 'cytoscape-cola';
cytoscape.use(cola);

// @ts-ignore
import dagre from 'cytoscape-dagre';
cytoscape.use(dagre);

// @ts-ignore
import fcose from 'cytoscape-fcose';
cytoscape.use(fcose);


export class GraphLayoutModel extends WidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      _model_name: 'GraphLayoutModel',
      _model_module: MODULE_NAME,
      _model_module_version: MODULE_VERSION,
      _view_name: 'GraphLayoutView',
      _view_module: MODULE_NAME,
      _view_module_version: MODULE_VERSION,

      selector: "*"
    };
  }
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
    this.model.on('msg:custom', this.handle_custom_message.bind(this));
  }

  handle_custom_message(content: any): void {
    switch (content.do) {
      case 'layout':
        this.render();
        break;
    }
  }

  render() {
    if (!this.cytoscape_obj) {
      this.el.innerHTML = "Cytoscape graph layout only usable in a Cytoscape widget.";
      return;
    }

    let eles = this.cytoscape_obj.filter(this.model.get('selector'));
    if (eles.nonempty()) return this.layout(eles);
  }

  layout(eles: any): Promise<void> {
    return Promise.resolve();
  }
}


export class NullLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      ...super.defaults(),

      _model_name: 'NullLayoutModel',
      _view_name: 'NullLayoutView'
    };
  }
}


export class NullLayoutView extends GraphLayoutView {
  layout(ele: any): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        let layout = ele.layout({
          name: 'null',
          stop: () => {
            layout.destroy();
            this.send({"type": "layout_stop"});
            resolve();
          }
        });

        layout.run();
      } catch (e) {
        reject(e);
      }
    });
  }
}


export class RandomLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      ...super.defaults(),

      _model_name: 'RandomLayoutModel',
      _view_name: 'RandomLayoutView',

      fit: true,
      padding: 30,
      bounding_box: null,
      animate: false,
      animation_duration: 500,
      animation_easing: null,
    };
  }
}


export class RandomLayoutView extends GraphLayoutView {
  layout(ele: any): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        let layout = ele.layout({
          name: 'random',
          fit: this.model.get('fit'),
          padding: this.model.get('padding'),
          boundingBox: this.model.get('bounding_box') || undefined,
          animate: this.model.get('animate'),
          animationDuration: this.model.get('animation_duration'),
          animationEasing: this.model.get('animation_easing') || undefined,
          stop: () => {
            layout.destroy();
            this.send({"type": "layout_stop"});
            resolve();
          }
        });

        layout.run();
      } catch (e) {
        reject(e);
      }
    });
  }
}


export class PresetLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      ...super.defaults(),

      _model_name: 'PresetLayoutModel',
      _view_name: 'PresetLayoutView',

      zoom: null,
      pan: null,
      fit: true,
      padding: 30,
      animate: false,
      animation_duration: 500,
      animation_easing: null,
    };
  }
}


export class PresetLayoutView extends GraphLayoutView {
  layout(ele: any): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        let layout = ele.layout({
          name: 'preset',
          zoom: this.model.get('zoom') || undefined,
          pan: this.model.get('pan') || undefined,
          fit: this.model.get('fit'),
          padding: this.model.get('padding'),
          position: (node: any) => node.data('position'),
          animate: this.model.get('animate'),
          animationDuration: this.model.get('animation_duration'),
          animationEasing: this.model.get('animation_easing') || undefined,
          stop: () => {
            layout.destroy();
            this.send({"type": "layout_stop"});
            resolve();
          }
        });

        layout.run();
      } catch (e) {
        reject(e);
      }
    });
  }
}


export class GridLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      ...super.defaults(),

      _model_name: 'GridLayoutModel',
      _view_name: 'GridLayoutView',

      animate: false,
      animation_duration: 500,
      animation_easing: null,
      avoid_overlap_padding: 10,
      avoid_overlap: true,
      bounding_box: null,
      cols: null,
      condense: false,
      fit: true,
      node_dimensions_include_labels: false,
      padding: 30,
      rows: null,
      spacing_factor: null
   };
  }
}


export class GridLayoutView extends GraphLayoutView {
  layout(ele: any): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        let layout = ele.layout({
          name: 'grid',
          animate: this.model.get('animate'),
          animationDuration: this.model.get('animation_duration'),
          animationEasing: this.model.get('animation_easing') || undefined,
          avoidOverlapPadding: this.model.get('avoid_overlap_padding'),
          avoidOverlap: this.model.get('avoid_overlap'),
          boundingBox: this.model.get('bounding_box') || undefined,
          cols: this.model.get('cols') || undefined,
          condense: this.model.get('condense'),
          fit: this.model.get('fit'),
          nodeDimensionsIncludeLabels: this.model.get('node_dimensions_include_labels'),
          padding: this.model.get('padding'),
          rows: this.model.get('rows') || undefined,
          spacingFactor: this.model.get('spacing_factor') || undefined,
          stop: () => {
            layout.destroy();
            this.send({"type": "layout_stop"});
            resolve();
          }
        });

        layout.run();
      } catch (e) {
        reject(e);
      }
    });
  }
}


export class CircleLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      ...super.defaults(),

      _model_name: 'CircleLayoutModel',
      _view_name: 'CircleLayoutView',

      animate: false,
      animation_duration: 500,
      animation_easing: null,
      avoid_overlap: true,
      bounding_box: null,
      clockwise: true,
      fit: true,
      node_dimensions_include_labels: false,
      padding: 30,
      radius: null,
      spacing_factor: null,
      start_angle: 3 / 2 * Math.PI,
      sweep: null
    };
  }
}


export class CircleLayoutView extends GraphLayoutView {
  layout(ele: any): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        let layout = ele.layout({
          name: 'circle',
          animate: this.model.get('animate'),
          animationDuration: this.model.get('animation_duration'),
          animationEasing: this.model.get('animation_easing') || undefined,
          avoidOverlap: this.model.get('avoid_overlap'),
          boundingBox: this.model.get('bounding_box') || undefined,
          clockwise: this.model.get('clockwise'),
          fit: this.model.get('fit'),
          nodeDimensionsIncludeLabels: this.model.get('node_dimensions_include_labels'),
          padding: this.model.get('padding'),
          radius: this.model.get('radius') || undefined,
          spacingFactor: this.model.get('spacing_factor') || undefined,
          startAngle: this.model.get('start_angle'),
          sweep: this.model.get('sweep') || undefined,
          stop: () => {
            layout.destroy();
            this.send({"type": "layout_stop"});
            resolve();
          }
        });

        layout.run();
      } catch (e) {
        reject(e);
      }
    });
  }
}


export class ConcentricLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      ...super.defaults(),

      _model_name: 'ConcentricLayoutModel',
      _view_name: 'ConcentricLayoutView',

      animate: false,
      animation_duration: 500,
      animation_easing: null,
      avoid_overlap: true,
      bounding_box: null,
      clockwise: true,
      equidistant: false,
      fit: true,
      min_node_spacing: 10,
      node_dimensions_include_labels: false,
      padding: 30,
      spacing_factor: null,
      start_angle: 3 / 2 * Math.PI,
      sweep: null
    };
  }
}


export class ConcentricLayoutView extends GraphLayoutView {
  layout(ele: any): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        let layout = ele.layout({
          name: 'concentric',
          animate: this.model.get('animate'),
          animationDuration: this.model.get('animation_duration'),
          animationEasing: this.model.get('animation_easing') || undefined,
          avoidOverlap: this.model.get('avoid_overlap'),
          boundingBox: this.model.get('bounding_box') || undefined,
          clockwise: this.model.get('clockwise'),
          equidistant: this.model.get('equidistant'),
          fit: this.model.get('fit'),
          minNodeSpacing: this.model.get('min_node_spacing'),
          nodeDimensionsIncludeLabels: this.model.get('node_dimensions_include_labels'),
          padding: this.model.get('padding'),
          spacingFactor: this.model.get('spacing_factor'),
          startAngle: this.model.get('start_angle'),
          sweep: this.model.get('sweep') || undefined,
          stop: () => {
            layout.destroy();
            this.send({"type": "layout_stop"});
            resolve();
          }
        });

        layout.run();
      } catch (e) {
        reject(e);
      }
    });
  }
}


export class BreadthFirstLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      ...super.defaults(),

      _model_name: 'BreadthFirstLayoutModel',
      _view_name: 'BreadthFirstLayoutView',

      animate: false,
      animation_duration: 500,
      animation_easing: null,
      avoid_overlap: true,
      bounding_box: null,
      circle: false,
      directed: false,
      fit: true,
      grid: false,
      maximal: false,
      node_dimensions_include_labels: false,
      padding: 30,
      roots: null,
      spacing_factor: 1.75
    };
  }
}


export class BreadthFirstLayoutView extends GraphLayoutView {
  layout(ele: any): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        let layout = ele.layout({
          name: 'breadthfirst',
          animate: this.model.get('animate'),
          animationDuration: this.model.get('animation_duration'),
          animationEasing: this.model.get('animation_easing') || undefined,
          avoidOverlap: this.model.get('avoid_overlap'),
          boundingBox: this.model.get('bounding_box') || undefined,
          circle: this.model.get('circle'),
          directed: this.model.get('directed'),
          fit: this.model.get('fit'),
          grid: this.model.get('grid'),
          maximal: this.model.get('maximal'),
          nodeDimensionsIncludeLabels: this.model.get('node_dimensions_include_labels'),
          padding: this.model.get('padding'),
          roots: this.model.get('roots') || undefined,
          spacingFactor: this.model.get('spacing_factor'),
          stop: () => {
            layout.destroy();
            this.send({"type": "layout_stop"});
            resolve();
          }
        });

        layout.run();
      } catch (e) {
        reject(e);
      }
    });
  }
}


export class CoseLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      ...super.defaults(),

      _model_name: 'CoseLayoutModel',
      _view_name: 'CoseLayoutView',

      animate: true,
      animation_duration: null,
      animation_easing: null,
      animation_threshold: 250,
      bounding_box: null,
      component_spacing: 40,
      cooling_factor: 0.99,
      edge_elasticity: 32,
      fit: true,
      gravity: 1,
      ideal_edge_length: 32,
      initial_temp: 1000,
      min_temp: 1.0,
      nesting_factor: 1.2,
      node_dimensions_include_labels: false,
      node_overlap: 4,
      node_repulsion: 2048,
      num_iter: 1000,
      padding: 30,
      randomize: false,
      refresh: 20
    };
  }
}


export class CoseLayoutView extends GraphLayoutView {
  layout(ele: any): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        let layout = ele.layout({
          name: 'cose',
          animate: this.model.get('animate'),
          animationDuration: this.model.get('animation_duration') || undefined,
          animationEasing: this.model.get('animation_easing') || undefined,
          animationThreshold: this.model.get('animation_threshold'),
          boundingBox: this.model.get('bounding_box') || undefined,
          componentSpacing: this.model.get('component_spacing'),
          coolingFactor: this.model.get('cooling_factor'),
          edgeElasticity: (edge: any) => edge.data('edge_elasticity') || this.model.get('edge_elasticity'),
          fit: this.model.get('fit'),
          gravity: this.model.get('gravity'),
          idealEdgeLength: (edge: any) => edge.data('ideal_edge_length') || this.model.get('ideal_edge_length'),
          initialTemp: this.model.get('initial_temp'),
          minTemp: this.model.get('min_temp'),
          nestingFactor: this.model.get('nesting_factor'),
          nodeDimensionsIncludeLabels: this.model.get('node_dimensions_include_labels'),
          nodeOverlap: this.model.get('node_overlap'),
          nodeRepulsion: (node: any) => node.data('node_repulsion') || this.model.get('node_repulsion'),
          numIter: this.model.get('num_iter'),
          padding: this.model.get('padding'),
          randomize: this.model.get('randomize'),
          refresh: this.model.get('refresh'),
          stop: () => {
            layout.destroy();
            this.send({"type": "layout_stop"});
            resolve();
          }
        });

        layout.run();
      } catch (e) {
        reject(e);
      }
    });
  }
}


export class ColaLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      ...super.defaults(),

      _model_name: 'ColaLayoutModel',
      _view_name: 'ColaLayoutView',

      alignment: null,
      all_const_iter: null,
      animate: true,
      avoid_overlap: true,
      bounding_box: null,
      convergence_threshold: 0.01,
      edge_jaccard_length: null,
      edge_length: null,
      edge_sym_diff_length: null,
      fit: true,
      flow: null,
      gap_inequalities: null,
      handle_disconnected: true,
      max_simulation_time: 4000,
      node_dimensions_include_labels: false,
      node_spacing: 10,
      padding: 30,
      randomize: false,
      refresh: 1,
      unconstr_iter: null,
      ungrabify_while_imulating: false,
      user_const_iter: null
    };
  }
}


export class ColaLayoutView extends GraphLayoutView {
  layout(ele: any): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        let layout = ele.layout({
          name: 'cola',
          alignment: (node: any) => node.data('alignment') || this.model.get('alignment') || undefined,
          allConstIter: this.model.get('all_const_iter') || undefined,
          animate: this.model.get('animate'),
          avoidOverlap: this.model.get('avoid_overlap'),
          boundingBox: this.model.get('bounding_box') || undefined,
          convergenceThreshold: this.model.get('animate'),
          edgeJaccardLength: (edge: any) => edge.data('edge_jaccard_length') || this.model.get('edge_jaccard_length') || undefined,
          edgeLength: (edge: any) => edge.data('edge_length') || this.model.get('edge_length') || undefined,
          edgeSymDiffLength: (edge: any) => edge.data('edge_sym_diff_length') || this.model.get('edge_sym_diff_length') || undefined,
          fit: this.model.get('fit'),
          flow: this.model.get('flow') || undefined,
          gapInequalities: this.model.get('gap_inequalities') || undefined,
          handleDisconnected: this.model.get('handle_disconnected'),
          maxSimulationTime: this.model.get('max_simulation_time'),
          nodeDimensionsIncludeLabels: this.model.get('node_dimensions_include_lablels'),
          nodeSpacing: (node: any) => node.data('node_spacing') || this.model.get('node_spacing'),
          padding: this.model.get('padding'),
          randomize: this.model.get('randomize'),
          refresh: this.model.get('refresh'),
          unconstrIter: this.model.get('unconstr_iter') || undefined,
          ungrabifyWhileSimulating: this.model.get('ungrabify_while_simulating'),
          userConstIter: this.model.get('user_const_iter') || undefined,
          stop: () => {
            layout.destroy();
            this.send({"type": "layout_stop"});
            resolve();
          }
        });

        layout.run();
      } catch (e) {
        reject(e);
      }
    });
  }
}


export class DagreLayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      ...super.defaults(),

      _model_name: 'DagreLayoutModel',
      _view_name: 'DagreLayoutView',

      animate: false,
      animation_duration: 500,
      animation_easing: null,
      bounding_box: null,
      edge_sep: null,
      edge_weight: 1,
      fit: true,
      min_len: 1,
      node_dimensions_include_labels: false,
      node_sep: null,
      padding: 30,
      rank_dir: null,
      ranker: null,
      rank_sep: null,
      spacing_factor: null
    };
  }
}


export class DagreLayoutView extends GraphLayoutView {
  layout(ele: any): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        let layout = ele.layout({
          name: 'dagre',
          animate: this.model.get('animate'),
          animationDuration: this.model.get('animation_duration'),
          animationEasing: this.model.get('animation_easing'),
          boundingBox: this.model.get('bounding_box'),
          edgeSep: this.model.get('edge_sep'),
          edgeWeight: (edge: any) => edge.data('edge_weight') || this.model.get('edge_weight'),
          fit: this.model.get('fit'),
          minLen: (edge: any) => edge.data('min_len') || this.model.get('min_len'),
          nodeDimensionsIncludeLabels: this.model.get('node_dimensions_include_labels'),
          nodeSep: this.model.get('node_sep'),
          padding: this.model.get('padding'),
          rankDir: this.model.get('rank_dir'),
          ranker: this.model.get('ranker'),
          rankSep: this.model.get('rank_sep'),
          spacingFactor: this.model.get('spacing_factor'),
          stop: () => {
            layout.destroy();
            this.send({"type": "layout_stop"});
            resolve();
          }
        });

        layout.run();
      } catch (e) {
        reject(e);
      }
    });
  }
}


export class FCoSE_LayoutModel extends GraphLayoutModel {
  defaults() {
    return {
      ...super.defaults(),

      _model_name: 'FCoSE_LayoutModel',
      _view_name: 'FCoSE_LayoutView',

      animate: true,
      animation_duration: 1000,
      animation_easing: undefined,
      edge_elasticity: 0.45,
      fit: true,
      gravity: 0.25,
      gravity_compound: 1.0,
      gravity_range: 3.8,
      gravity_range_compound: 1.5,
      ideal_edge_length: 50,
      initial_energy_on_incremental: 0.3,
      nesting_factor: 0.1,
      node_dimensions_include_labels: false,
      node_repulsion: 4500,
      node_separation: 75,
      num_iter: 2500,
      pack_components: true,
      padding: 30,
      pi_tol: 0.0000001,
      quality: "default",
      randomize: true,
      sample_size: 25,
      sampling_type: true,
      tile: true,
      tiling_padding_horizontal: 10,
      tiling_padding_vertical: 10,
      uniform_node_dimensions: false
    };
  }
}


export class FCoSE_LayoutView extends GraphLayoutView {
  layout(ele: any): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        let layout = ele.layout({
          name: 'fcose',

          animate: this.model.get('animate'),
          animationDuration: this.model.get('animation_duration'),
          animationEasing: this.model.get('animation_easing') || undefined,
          edgeElasticity: this.model.get('edge_elasticity'),
          fit: this.model.get('fit'),
          gravityCompound: this.model.get('gravity_range_compound'),
          gravityRangeCompound: this.model.get('gravity_range_compound'),
          gravityRange: this.model.get('gravity_range'),
          gravity: this.model.get('gravity'),
          idealEdgeLength: this.model.get('ideal_edge_length'),
          initialEnergyOnIncremental: this.model.get('initial_energy_on_incremental'),
          nestingFactor: this.model.get('nesting_factor'),
          nodeDimensionsIncludeLabels: this.model.get('node_dimensions_include_labels'),
          nodeRepulsion: this.model.get('node_repulsion'),
          nodeSeparation: this.model.get('node_separation'),
          numIter: this.model.get('num_iter'),
          packComponents: this.model.get('pack_components'),
          padding: this.model.get('padding'),
          piTol: this.model.get('pi_tol'),
          quality: this.model.get('quality'),
          randomize: this.model.get('randomize'),
          sampleSize: this.model.get('sample_size'),
          samplingType: this.model.get('sampling_type'),
          tile: this.model.get('tile'),
          tilingPaddingHorizontal: this.model.get('tiling_padding_horizontal'),
          tilingPaddingVertical: this.model.get('tiling_padding_vertical'),
          uniformNodeDimensions: this.model.get('uniform_node_dimensions'),

          stop: () => {
            layout.destroy();
            this.send({"type": "layout_stop"});
            resolve();
          }
        });

        layout.run();
      } catch (e) {
        reject(e);
      }
    });
  }
}

