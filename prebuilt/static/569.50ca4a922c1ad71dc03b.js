"use strict";(self.webpackChunkcytoscape_clj=self.webpackChunkcytoscape_clj||[]).push([[569],{3569:(e,t,i)=>{i.r(t),i.d(t,{BreadthFirstLayoutModel:()=>V,BreadthFirstLayoutView:()=>P,CircleLayoutModel:()=>C,CircleLayoutView:()=>D,ColaLayoutModel:()=>I,ColaLayoutView:()=>F,ConcentricLayoutModel:()=>M,ConcentricLayoutView:()=>k,CoseLayoutModel:()=>z,CoseLayoutView:()=>S,DagreLayoutModel:()=>B,DagreLayoutView:()=>R,FCoSE_LayoutModel:()=>O,FCoSE_LayoutView:()=>N,GraphLayoutModel:()=>h,GraphLayoutView:()=>p,GridLayoutModel:()=>x,GridLayoutView:()=>E,KlayLayoutModel:()=>G,KlayLayoutView:()=>T,NullLayoutModel:()=>y,NullLayoutView:()=>f,PresetLayoutModel:()=>w,PresetLayoutView:()=>L,RandomLayoutModel:()=>v,RandomLayoutView:()=>b});var n=i(8947),o=i(8657),a=i(2099),d=i.n(a),s=i(2696),l=i.n(s),m=i(9299),g=i.n(m),r=i(6391),_=i.n(r),u=i(9354),c=i.n(u);d().use(l()),d().use(g()),d().use(_()),d().use(c());class h extends n.WidgetModel{defaults(){return{...super.defaults(),_model_name:"GraphLayoutModel",_model_module:o.o,_model_module_version:o.Y,_view_name:"GraphLayoutView",_view_module:o.o,_view_module_version:o.Y,selector:"*"}}}class p extends n.WidgetView{constructor(e){super({model:e.model,options:e.options}),this.cytoscape_obj=this.options.cytoscape_obj,this.model.on("change",this.render.bind(this),this),this.model.on("msg:custom",this.handle_custom_message.bind(this))}handle_custom_message(e){"layout"===e.do&&this.render()}render(){if(!this.cytoscape_obj)return void(this.el.innerHTML="Cytoscape graph layout only usable in a Cytoscape widget.");let e=this.cytoscape_obj.filter(this.model.get("selector"));return e.nonempty()?this.layout(e):void 0}layout(e){return Promise.resolve()}}class y extends h{defaults(){return{...super.defaults(),_model_name:"NullLayoutModel",_view_name:"NullLayoutView"}}}class f extends p{layout(e){return new Promise(((t,i)=>{try{let i=e.layout({name:"null",stop:()=>{i.destroy(),this.send({type:"layout_stop"}),t()}});i.run()}catch(e){i(e)}}))}}class v extends h{defaults(){return{...super.defaults(),_model_name:"RandomLayoutModel",_view_name:"RandomLayoutView",fit:!0,padding:30,bounding_box:null,animate:!1,animation_duration:500,animation_easing:null}}}class b extends p{layout(e){return new Promise(((t,i)=>{try{let i=e.layout({name:"random",fit:this.model.get("fit"),padding:this.model.get("padding"),boundingBox:this.model.get("bounding_box")||void 0,animate:this.model.get("animate"),animationDuration:this.model.get("animation_duration"),animationEasing:this.model.get("animation_easing")||void 0,stop:()=>{i.destroy(),this.send({type:"layout_stop"}),t()}});i.run()}catch(e){i(e)}}))}}class w extends h{defaults(){return{...super.defaults(),_model_name:"PresetLayoutModel",_view_name:"PresetLayoutView",zoom:null,pan:null,fit:!0,padding:30,animate:!1,animation_duration:500,animation_easing:null}}}class L extends p{layout(e){return new Promise(((t,i)=>{try{let i=e.layout({name:"preset",zoom:this.model.get("zoom")||void 0,pan:this.model.get("pan")||void 0,fit:this.model.get("fit"),padding:this.model.get("padding"),position:e=>e.data("position"),animate:this.model.get("animate"),animationDuration:this.model.get("animation_duration"),animationEasing:this.model.get("animation_easing")||void 0,stop:()=>{i.destroy(),this.send({type:"layout_stop"}),t()}});i.run()}catch(e){i(e)}}))}}class x extends h{defaults(){return{...super.defaults(),_model_name:"GridLayoutModel",_view_name:"GridLayoutView",animate:!1,animation_duration:500,animation_easing:null,avoid_overlap_padding:10,avoid_overlap:!0,bounding_box:null,cols:null,condense:!1,fit:!0,node_dimensions_include_labels:!1,padding:30,rows:null,spacing_factor:null}}}class E extends p{layout(e){return new Promise(((t,i)=>{try{let i=e.layout({name:"grid",animate:this.model.get("animate"),animationDuration:this.model.get("animation_duration"),animationEasing:this.model.get("animation_easing")||void 0,avoidOverlapPadding:this.model.get("avoid_overlap_padding"),avoidOverlap:this.model.get("avoid_overlap"),boundingBox:this.model.get("bounding_box")||void 0,cols:this.model.get("cols")||void 0,condense:this.model.get("condense"),fit:this.model.get("fit"),nodeDimensionsIncludeLabels:this.model.get("node_dimensions_include_labels"),padding:this.model.get("padding"),rows:this.model.get("rows")||void 0,spacingFactor:this.model.get("spacing_factor")||void 0,stop:()=>{i.destroy(),this.send({type:"layout_stop"}),t()}});i.run()}catch(e){i(e)}}))}}class C extends h{defaults(){return{...super.defaults(),_model_name:"CircleLayoutModel",_view_name:"CircleLayoutView",animate:!1,animation_duration:500,animation_easing:null,avoid_overlap:!0,bounding_box:null,clockwise:!0,fit:!0,node_dimensions_include_labels:!1,padding:30,radius:null,spacing_factor:null,start_angle:1.5*Math.PI,sweep:null}}}class D extends p{layout(e){return new Promise(((t,i)=>{try{let i=e.layout({name:"circle",animate:this.model.get("animate"),animationDuration:this.model.get("animation_duration"),animationEasing:this.model.get("animation_easing")||void 0,avoidOverlap:this.model.get("avoid_overlap"),boundingBox:this.model.get("bounding_box")||void 0,clockwise:this.model.get("clockwise"),fit:this.model.get("fit"),nodeDimensionsIncludeLabels:this.model.get("node_dimensions_include_labels"),padding:this.model.get("padding"),radius:this.model.get("radius")||void 0,spacingFactor:this.model.get("spacing_factor")||void 0,startAngle:this.model.get("start_angle"),sweep:this.model.get("sweep")||void 0,stop:()=>{i.destroy(),this.send({type:"layout_stop"}),t()}});i.run()}catch(e){i(e)}}))}}class M extends h{defaults(){return{...super.defaults(),_model_name:"ConcentricLayoutModel",_view_name:"ConcentricLayoutView",animate:!1,animation_duration:500,animation_easing:null,avoid_overlap:!0,bounding_box:null,clockwise:!0,equidistant:!1,fit:!0,min_node_spacing:10,node_dimensions_include_labels:!1,padding:30,spacing_factor:null,start_angle:1.5*Math.PI,sweep:null}}}class k extends p{layout(e){return new Promise(((t,i)=>{try{let i=e.layout({name:"concentric",animate:this.model.get("animate"),animationDuration:this.model.get("animation_duration"),animationEasing:this.model.get("animation_easing")||void 0,avoidOverlap:this.model.get("avoid_overlap"),boundingBox:this.model.get("bounding_box")||void 0,clockwise:this.model.get("clockwise"),equidistant:this.model.get("equidistant"),fit:this.model.get("fit"),minNodeSpacing:this.model.get("min_node_spacing"),nodeDimensionsIncludeLabels:this.model.get("node_dimensions_include_labels"),padding:this.model.get("padding"),spacingFactor:this.model.get("spacing_factor"),startAngle:this.model.get("start_angle"),sweep:this.model.get("sweep")||void 0,stop:()=>{i.destroy(),this.send({type:"layout_stop"}),t()}});i.run()}catch(e){i(e)}}))}}class V extends h{defaults(){return{...super.defaults(),_model_name:"BreadthFirstLayoutModel",_view_name:"BreadthFirstLayoutView",animate:!1,animation_duration:500,animation_easing:null,avoid_overlap:!0,bounding_box:null,circle:!1,directed:!1,fit:!0,grid:!1,maximal:!1,node_dimensions_include_labels:!1,padding:30,roots:null,spacing_factor:1.75}}}class P extends p{layout(e){return new Promise(((t,i)=>{try{let i=e.layout({name:"breadthfirst",animate:this.model.get("animate"),animationDuration:this.model.get("animation_duration"),animationEasing:this.model.get("animation_easing")||void 0,avoidOverlap:this.model.get("avoid_overlap"),boundingBox:this.model.get("bounding_box")||void 0,circle:this.model.get("circle"),directed:this.model.get("directed"),fit:this.model.get("fit"),grid:this.model.get("grid"),maximal:this.model.get("maximal"),nodeDimensionsIncludeLabels:this.model.get("node_dimensions_include_labels"),padding:this.model.get("padding"),roots:this.model.get("roots")||void 0,spacingFactor:this.model.get("spacing_factor"),stop:()=>{i.destroy(),this.send({type:"layout_stop"}),t()}});i.run()}catch(e){i(e)}}))}}class z extends h{defaults(){return{...super.defaults(),_model_name:"CoseLayoutModel",_view_name:"CoseLayoutView",animate:!0,animation_duration:null,animation_easing:null,animation_threshold:250,bounding_box:null,component_spacing:40,cooling_factor:.99,edge_elasticity:32,fit:!0,gravity:1,ideal_edge_length:32,initial_temp:1e3,min_temp:1,nesting_factor:1.2,node_dimensions_include_labels:!1,node_overlap:4,node_repulsion:2048,num_iter:1e3,padding:30,randomize:!1,refresh:20}}}class S extends p{layout(e){return new Promise(((t,i)=>{try{let i=e.layout({name:"cose",animate:this.model.get("animate"),animationDuration:this.model.get("animation_duration")||void 0,animationEasing:this.model.get("animation_easing")||void 0,animationThreshold:this.model.get("animation_threshold"),boundingBox:this.model.get("bounding_box")||void 0,componentSpacing:this.model.get("component_spacing"),coolingFactor:this.model.get("cooling_factor"),edgeElasticity:e=>e.data("edge_elasticity")||this.model.get("edge_elasticity"),fit:this.model.get("fit"),gravity:this.model.get("gravity"),idealEdgeLength:e=>e.data("ideal_edge_length")||this.model.get("ideal_edge_length"),initialTemp:this.model.get("initial_temp"),minTemp:this.model.get("min_temp"),nestingFactor:this.model.get("nesting_factor"),nodeDimensionsIncludeLabels:this.model.get("node_dimensions_include_labels"),nodeOverlap:this.model.get("node_overlap"),nodeRepulsion:e=>e.data("node_repulsion")||this.model.get("node_repulsion"),numIter:this.model.get("num_iter"),padding:this.model.get("padding"),randomize:this.model.get("randomize"),refresh:this.model.get("refresh"),stop:()=>{i.destroy(),this.send({type:"layout_stop"}),t()}});i.run()}catch(e){i(e)}}))}}class I extends h{defaults(){return{...super.defaults(),_model_name:"ColaLayoutModel",_view_name:"ColaLayoutView",alignment:null,all_const_iter:null,animate:!0,avoid_overlap:!0,bounding_box:null,convergence_threshold:.01,edge_jaccard_length:null,edge_length:null,edge_sym_diff_length:null,fit:!0,flow:null,gap_inequalities:null,handle_disconnected:!0,max_simulation_time:4e3,node_dimensions_include_labels:!1,node_spacing:10,padding:30,randomize:!1,refresh:1,unconstr_iter:null,ungrabify_while_imulating:!1,user_const_iter:null}}}class F extends p{layout(e){return new Promise(((t,i)=>{try{let i=e.layout({name:"cola",alignment:e=>e.data("alignment")||this.model.get("alignment")||void 0,allConstIter:this.model.get("all_const_iter")||void 0,animate:this.model.get("animate"),avoidOverlap:this.model.get("avoid_overlap"),boundingBox:this.model.get("bounding_box")||void 0,convergenceThreshold:this.model.get("animate"),edgeJaccardLength:e=>e.data("edge_jaccard_length")||this.model.get("edge_jaccard_length")||void 0,edgeLength:e=>e.data("edge_length")||this.model.get("edge_length")||void 0,edgeSymDiffLength:e=>e.data("edge_sym_diff_length")||this.model.get("edge_sym_diff_length")||void 0,fit:this.model.get("fit"),flow:this.model.get("flow")||void 0,gapInequalities:this.model.get("gap_inequalities")||void 0,handleDisconnected:this.model.get("handle_disconnected"),maxSimulationTime:this.model.get("max_simulation_time"),nodeDimensionsIncludeLabels:this.model.get("node_dimensions_include_lablels"),nodeSpacing:e=>e.data("node_spacing")||this.model.get("node_spacing"),padding:this.model.get("padding"),randomize:this.model.get("randomize"),refresh:this.model.get("refresh"),unconstrIter:this.model.get("unconstr_iter")||void 0,ungrabifyWhileSimulating:this.model.get("ungrabify_while_simulating"),userConstIter:this.model.get("user_const_iter")||void 0,stop:()=>{i.destroy(),this.send({type:"layout_stop"}),t()}});i.run()}catch(e){i(e)}}))}}class B extends h{defaults(){return{...super.defaults(),_model_name:"DagreLayoutModel",_view_name:"DagreLayoutView",animate:!1,animation_duration:500,animation_easing:null,bounding_box:null,edge_sep:null,edge_weight:1,fit:!0,min_len:1,node_dimensions_include_labels:!1,node_sep:null,padding:30,rank_dir:null,ranker:null,rank_sep:null,spacing_factor:null}}}class R extends p{layout(e){return new Promise(((t,i)=>{try{let i=e.layout({name:"dagre",animate:this.model.get("animate"),animationDuration:this.model.get("animation_duration"),animationEasing:this.model.get("animation_easing"),boundingBox:this.model.get("bounding_box"),edgeSep:this.model.get("edge_sep"),edgeWeight:e=>e.data("edge_weight")||this.model.get("edge_weight"),fit:this.model.get("fit"),minLen:e=>e.data("min_len")||this.model.get("min_len"),nodeDimensionsIncludeLabels:this.model.get("node_dimensions_include_labels"),nodeSep:this.model.get("node_sep"),padding:this.model.get("padding"),rankDir:this.model.get("rank_dir"),ranker:this.model.get("ranker"),rankSep:this.model.get("rank_sep"),spacingFactor:this.model.get("spacing_factor"),stop:()=>{i.destroy(),this.send({type:"layout_stop"}),t()}});i.run()}catch(e){i(e)}}))}}class O extends h{defaults(){return{...super.defaults(),_model_name:"FCoSE_LayoutModel",_view_name:"FCoSE_LayoutView",animate:!0,animation_duration:1e3,animation_easing:void 0,edge_elasticity:.45,fit:!0,gravity:.25,gravity_compound:1,gravity_range:3.8,gravity_range_compound:1.5,ideal_edge_length:50,initial_energy_on_incremental:.3,nesting_factor:.1,node_dimensions_include_labels:!1,node_repulsion:4500,node_separation:75,num_iter:2500,pack_components:!0,padding:30,pi_tol:1e-7,quality:"default",randomize:!0,sample_size:25,sampling_type:!0,tile:!0,tiling_padding_horizontal:10,tiling_padding_vertical:10,uniform_node_dimensions:!1}}}class N extends p{layout(e){return new Promise(((t,i)=>{try{let i=e.layout({name:"fcose",animate:this.model.get("animate"),animationDuration:this.model.get("animation_duration"),animationEasing:this.model.get("animation_easing")||void 0,edgeElasticity:this.model.get("edge_elasticity"),fit:this.model.get("fit"),gravityCompound:this.model.get("gravity_range_compound"),gravityRangeCompound:this.model.get("gravity_range_compound"),gravityRange:this.model.get("gravity_range"),gravity:this.model.get("gravity"),idealEdgeLength:this.model.get("ideal_edge_length"),initialEnergyOnIncremental:this.model.get("initial_energy_on_incremental"),nestingFactor:this.model.get("nesting_factor"),nodeDimensionsIncludeLabels:this.model.get("node_dimensions_include_labels"),nodeRepulsion:this.model.get("node_repulsion"),nodeSeparation:this.model.get("node_separation"),numIter:this.model.get("num_iter"),packComponents:this.model.get("pack_components"),padding:this.model.get("padding"),piTol:this.model.get("pi_tol"),quality:this.model.get("quality"),randomize:this.model.get("randomize"),sampleSize:this.model.get("sample_size"),samplingType:this.model.get("sampling_type"),tile:this.model.get("tile"),tilingPaddingHorizontal:this.model.get("tiling_padding_horizontal"),tilingPaddingVertical:this.model.get("tiling_padding_vertical"),uniformNodeDimensions:this.model.get("uniform_node_dimensions"),stop:()=>{i.destroy(),this.send({type:"layout_stop"}),t()}});i.run()}catch(e){i(e)}}))}}class G extends h{defaults(){return{...super.defaults(),_model_name:"KlayLayoutModel",_view_name:"KlayLayoutView",animate:!0,animation_duration:1e3,animation_easing:void 0,fit:!0,node_dimensions_include_labels:!1,padding:30,add_unnecessary_bendpoints:!1,aspect_ratio:1.6,border_spacing:20,compact_components:!1,crossing_minimization:"LAYER_SWEEP",cycle_breaking:"GREEDY",direction:"UNDEFINED",edge_routing:"ORTHOGONAL",edge_spacing_factor:.5,feedback_edges:!1,fixed_alignment:"NONE",in_layer_spacing_factor:1,layout_hierarchy:!1,linear_segments_deflection_dampening:.3,merge_edges:!1,merge_hierarchy_crossing_edges:!0,node_layering:"NETWORK_SIMPLEX",node_placement:"BRANDES_KOEPF",randomization_seed:1,route_self_loop_inside:!1,separate_connected_components:!0,spacing:20,thoroughness:7}}}class T extends p{layout(e){return new Promise(((t,i)=>{try{let i=e.layout({name:"klay",animate:this.model.get("animate"),animationDuration:this.model.get("animation_duration"),animationEasing:this.model.get("animation_easing")||void 0,fit:this.model.get("fit"),nodeDimensionsIncludeLabels:this.model.get("node_dimensions_include_labels"),padding:this.model.get("padding"),klay:{addUnnecessaryBendpoints:this.model.get("add_unnecessary_bendpoints"),aspectRatio:this.model.get("aspect_ratio"),borderSpacing:this.model.get("border_spacing"),compactComponents:this.model.get("compact_components"),crossingMinimization:this.model.get("crossing_minimization"),cycleBreaking:this.model.get("cycle_breaking"),direction:this.model.get("direction"),edgeRouting:this.model.get("edge_routing"),edgeSpacingFactor:this.model.get("edge_spacing_factor"),feedbackEdges:this.model.get("feedback_edges"),fixedAlignment:this.model.get("fixed_alignment"),inLayerSpacingFactor:this.model.get("in_layer_spacing_factor"),layoutHierarchy:this.model.get("layout_hierarchy"),linearSegmentsDeflectionDampening:this.model.get("linear_segments_deflection_dampening"),mergeEdges:this.model.get("merge_edges"),mergeHierarchyCrossingEdges:this.model.get("merge_hierarchy_crossing_edges"),nodeLayering:this.model.get("node_layering"),nodePlacement:this.model.get("node_placement"),randomizationSeed:this.model.get("randomization_seed"),routeSelfLoopInside:this.model.get("route_self_loop_inside"),separateConnectedComponents:this.model.get("separate_connected_components"),spacing:this.model.get("spacing"),thoroughness:this.model.get("thoroughness")},stop:()=>{i.destroy(),this.send({type:"layout_stop"}),t()}});i.run()}catch(e){i(e)}}))}}}}]);