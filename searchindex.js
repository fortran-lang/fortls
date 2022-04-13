Search.setIndex({docnames:["README","editor_integration","fortls","fortls_changes","index","modules","options"],envversion:{"sphinx.domains.c":2,"sphinx.domains.changeset":1,"sphinx.domains.citation":1,"sphinx.domains.cpp":5,"sphinx.domains.index":1,"sphinx.domains.javascript":2,"sphinx.domains.math":2,"sphinx.domains.python":3,"sphinx.domains.rst":2,"sphinx.domains.std":2,"sphinx.ext.intersphinx":1,sphinx:56},filenames:["README.md","editor_integration.rst","fortls.rst","fortls_changes.md","index.rst","modules.rst","options.rst"],objects:{"":[[2,0,0,"-","fortls"]],"fortls.constants":[[2,1,1,"","FORTRAN_LITERAL"],[2,2,1,"","Severity"]],"fortls.constants.Severity":[[2,3,1,"","error"],[2,3,1,"","info"],[2,3,1,"","warn"]],"fortls.ftypes":[[2,2,1,"","CLASS_info"],[2,2,1,"","FUN_sig"],[2,2,1,"","GEN_info"],[2,2,1,"","INCLUDE_info"],[2,2,1,"","INT_info"],[2,2,1,"","RESULT_sig"],[2,2,1,"","Range"],[2,2,1,"","SELECT_info"],[2,2,1,"","SMOD_info"],[2,2,1,"","SUB_info"],[2,2,1,"","USE_info"],[2,2,1,"","VAR_info"],[2,2,1,"","VIS_info"]],"fortls.ftypes.CLASS_info":[[2,3,1,"","keywords"],[2,3,1,"","name"],[2,3,1,"","parent"]],"fortls.ftypes.FUN_sig":[[2,3,1,"","result"]],"fortls.ftypes.GEN_info":[[2,3,1,"","bound_name"],[2,3,1,"","pro_links"],[2,3,1,"","vis_flag"]],"fortls.ftypes.INCLUDE_info":[[2,3,1,"","file"],[2,3,1,"","line_number"],[2,3,1,"","path"],[2,3,1,"","scope_objs"]],"fortls.ftypes.INT_info":[[2,3,1,"","abstract"],[2,3,1,"","name"]],"fortls.ftypes.RESULT_sig":[[2,3,1,"","keywords"],[2,3,1,"","name"],[2,3,1,"","type"]],"fortls.ftypes.Range":[[2,3,1,"","end"],[2,3,1,"","start"]],"fortls.ftypes.SELECT_info":[[2,3,1,"","binding"],[2,3,1,"","desc"],[2,3,1,"","type"]],"fortls.ftypes.SMOD_info":[[2,3,1,"","name"],[2,3,1,"","parent"]],"fortls.ftypes.SUB_info":[[2,3,1,"","args"],[2,3,1,"","keywords"],[2,3,1,"","mod_flag"],[2,3,1,"","name"]],"fortls.ftypes.USE_info":[[2,3,1,"","mod_name"],[2,3,1,"","only_list"],[2,3,1,"","rename_map"]],"fortls.ftypes.VAR_info":[[2,3,1,"","keywords"],[2,3,1,"","var_names"],[2,3,1,"","var_type"]],"fortls.ftypes.VIS_info":[[2,3,1,"","obj_names"],[2,3,1,"","type"]],"fortls.helper_functions":[[2,4,1,"","detect_fixed_format"],[2,4,1,"","expand_name"],[2,4,1,"","find_paren_match"],[2,4,1,"","find_word_in_line"],[2,4,1,"","get_keywords"],[2,4,1,"","get_line_prefix"],[2,4,1,"","get_paren_level"],[2,4,1,"","get_paren_substring"],[2,4,1,"","get_var_stack"],[2,4,1,"","map_keywords"],[2,4,1,"","only_dirs"],[2,4,1,"","resolve_globs"],[2,4,1,"","separate_def_list"],[2,4,1,"","set_keyword_ordering"],[2,4,1,"","strip_line_label"],[2,4,1,"","strip_strings"]],"fortls.interface":[[2,2,1,"","SetAction"],[2,4,1,"","commandline_args"]],"fortls.intrinsics":[[2,2,1,"","fortran_intrinsic_obj"],[2,4,1,"","get_intrinsic_keywords"],[2,4,1,"","load_intrinsics"],[2,4,1,"","set_lowercase_intrinsics"]],"fortls.intrinsics.fortran_intrinsic_obj":[[2,5,1,"","get_desc"],[2,5,1,"","get_hover"],[2,5,1,"","get_signature"],[2,5,1,"","get_snippet"],[2,5,1,"","get_type"],[2,5,1,"","is_callable"]],"fortls.json_templates":[[2,4,1,"","change_json"],[2,4,1,"","diagnostic_json"],[2,4,1,"","location_json"],[2,4,1,"","range_json"],[2,4,1,"","symbol_json"],[2,4,1,"","uri_json"]],"fortls.jsonrpc":[[2,2,1,"","JSONRPC2Connection"],[2,6,1,"","JSONRPC2ProtocolError"],[2,2,1,"","ReadWriter"],[2,2,1,"","TCPReadWriter"],[2,4,1,"","deque_find_and_pop"],[2,4,1,"","path_from_uri"],[2,4,1,"","path_to_uri"],[2,4,1,"","read_rpc_messages"],[2,4,1,"","write_rpc_notification"],[2,4,1,"","write_rpc_request"]],"fortls.jsonrpc.JSONRPC2Connection":[[2,5,1,"","read_message"],[2,5,1,"","send_notification"],[2,5,1,"","send_request"],[2,5,1,"","send_request_batch"],[2,5,1,"","write_error"],[2,5,1,"","write_response"]],"fortls.jsonrpc.ReadWriter":[[2,5,1,"","read"],[2,5,1,"","readline"],[2,5,1,"","write"]],"fortls.jsonrpc.TCPReadWriter":[[2,5,1,"","read"],[2,5,1,"","readline"],[2,5,1,"","write"]],"fortls.langserver":[[2,6,1,"","JSONRPC2Error"],[2,2,1,"","LangServer"]],"fortls.langserver.LangServer":[[2,5,1,"","file_init"],[2,5,1,"","get_all_references"],[2,5,1,"","get_definition"],[2,5,1,"","get_diagnostics"],[2,5,1,"","handle"],[2,5,1,"","post_message"],[2,5,1,"","run"],[2,5,1,"","send_diagnostics"],[2,5,1,"","serve_autocomplete"],[2,5,1,"","serve_codeActions"],[2,5,1,"","serve_default"],[2,5,1,"","serve_definition"],[2,5,1,"","serve_document_symbols"],[2,5,1,"","serve_exit"],[2,5,1,"","serve_hover"],[2,5,1,"","serve_implementation"],[2,5,1,"","serve_initialize"],[2,5,1,"","serve_onChange"],[2,5,1,"","serve_onClose"],[2,5,1,"","serve_onOpen"],[2,5,1,"","serve_onSave"],[2,5,1,"","serve_references"],[2,5,1,"","serve_rename"],[2,5,1,"","serve_signature"],[2,5,1,"","serve_workspace_symbol"],[2,5,1,"","update_workspace_file"],[2,5,1,"","workspace_init"]],"fortls.objects":[[2,2,1,"","USE_line"],[2,4,1,"","climb_type_tree"],[2,4,1,"","find_in_scope"],[2,4,1,"","find_in_workspace"],[2,2,1,"","fortran_associate"],[2,2,1,"","fortran_ast"],[2,2,1,"","fortran_block"],[2,2,1,"","fortran_diagnostic"],[2,2,1,"","fortran_do"],[2,2,1,"","fortran_enum"],[2,2,1,"","fortran_function"],[2,2,1,"","fortran_if"],[2,2,1,"","fortran_include"],[2,2,1,"","fortran_int"],[2,2,1,"","fortran_meth"],[2,2,1,"","fortran_module"],[2,2,1,"","fortran_obj"],[2,2,1,"","fortran_program"],[2,2,1,"","fortran_scope"],[2,2,1,"","fortran_select"],[2,2,1,"","fortran_submodule"],[2,2,1,"","fortran_subroutine"],[2,2,1,"","fortran_type"],[2,2,1,"","fortran_var"],[2,2,1,"","fortran_where"],[2,4,1,"","get_use_tree"]],"fortls.objects.fortran_associate":[[2,5,1,"","create_binding_variable"],[2,5,1,"","get_desc"],[2,5,1,"","get_type"],[2,5,1,"","require_link"],[2,5,1,"","resolve_link"]],"fortls.objects.fortran_ast":[[2,5,1,"","add_doc"],[2,5,1,"","add_error"],[2,5,1,"","add_include"],[2,5,1,"","add_int_member"],[2,5,1,"","add_private"],[2,5,1,"","add_public"],[2,5,1,"","add_scope"],[2,5,1,"","add_use"],[2,5,1,"","add_variable"],[2,5,1,"","check_file"],[2,5,1,"","close_file"],[2,5,1,"","create_none_scope"],[2,5,1,"","end_ppif"],[2,5,1,"","end_scope"],[2,5,1,"","get_enc_scope_name"],[2,5,1,"","get_inner_scope"],[2,5,1,"","get_object"],[2,5,1,"","get_scopes"],[2,5,1,"","resolve_includes"],[2,5,1,"","resolve_links"],[2,5,1,"","start_ppif"]],"fortls.objects.fortran_block":[[2,5,1,"","get_children"],[2,5,1,"","get_desc"],[2,5,1,"","get_type"],[2,5,1,"","req_named_end"]],"fortls.objects.fortran_diagnostic":[[2,5,1,"","add_related"],[2,5,1,"","build"]],"fortls.objects.fortran_do":[[2,5,1,"","get_desc"],[2,5,1,"","get_type"]],"fortls.objects.fortran_enum":[[2,5,1,"","get_desc"],[2,5,1,"","get_type"]],"fortls.objects.fortran_function":[[2,5,1,"","copy_interface"],[2,5,1,"","get_desc"],[2,5,1,"","get_hover"],[2,5,1,"","get_interface"],[2,5,1,"","get_type"],[2,5,1,"","is_callable"],[2,5,1,"","resolve_link"]],"fortls.objects.fortran_if":[[2,5,1,"","get_desc"],[2,5,1,"","get_type"]],"fortls.objects.fortran_include":[[2,5,1,"","get_desc"]],"fortls.objects.fortran_int":[[2,5,1,"","get_desc"],[2,5,1,"","get_type"],[2,5,1,"","is_abstract"],[2,5,1,"","is_callable"],[2,5,1,"","is_external_int"],[2,5,1,"","require_link"],[2,5,1,"","resolve_link"]],"fortls.objects.fortran_meth":[[2,5,1,"","check_definition"],[2,5,1,"","get_documentation"],[2,5,1,"","get_hover"],[2,5,1,"","get_interface"],[2,5,1,"","get_signature"],[2,5,1,"","get_snippet"],[2,5,1,"","get_type"],[2,5,1,"","is_callable"],[2,5,1,"","resolve_link"],[2,5,1,"","set_parent"]],"fortls.objects.fortran_module":[[2,5,1,"","check_valid_parent"],[2,5,1,"","get_desc"],[2,5,1,"","get_type"]],"fortls.objects.fortran_obj":[[2,5,1,"","add_doc"],[2,5,1,"","check_definition"],[2,5,1,"","check_valid_parent"],[2,5,1,"","end"],[2,5,1,"","get_actions"],[2,5,1,"","get_ancestors"],[2,5,1,"","get_children"],[2,5,1,"","get_desc"],[2,5,1,"","get_diagnostics"],[2,5,1,"","get_documentation"],[2,5,1,"","get_hover"],[2,5,1,"","get_implicit"],[2,5,1,"","get_interface"],[2,5,1,"","get_placeholders"],[2,5,1,"","get_signature"],[2,5,1,"","get_snippet"],[2,5,1,"","get_type"],[2,5,1,"","get_type_obj"],[2,5,1,"","is_abstract"],[2,5,1,"","is_callable"],[2,5,1,"","is_external_int"],[2,5,1,"","is_mod_scope"],[2,5,1,"","is_optional"],[2,5,1,"","req_named_end"],[2,5,1,"","require_inherit"],[2,5,1,"","require_link"],[2,5,1,"","resolve_inherit"],[2,5,1,"","resolve_link"],[2,5,1,"","set_default_vis"],[2,5,1,"","set_parent"],[2,5,1,"","set_visibility"],[2,5,1,"","update_fqsn"]],"fortls.objects.fortran_program":[[2,5,1,"","get_desc"]],"fortls.objects.fortran_scope":[[2,5,1,"","add_child"],[2,5,1,"","add_member"],[2,5,1,"","add_subroutine"],[2,5,1,"","add_use"],[2,5,1,"","check_definitions"],[2,5,1,"","check_use"],[2,5,1,"","copy_from"],[2,5,1,"","get_children"],[2,5,1,"","mark_contains"],[2,5,1,"","set_implicit"],[2,5,1,"","set_inherit"],[2,5,1,"","set_parent"],[2,5,1,"","update_fqsn"]],"fortls.objects.fortran_select":[[2,5,1,"","create_binding_variable"],[2,5,1,"","get_desc"],[2,5,1,"","get_type"],[2,5,1,"","is_type_binding"],[2,5,1,"","is_type_region"]],"fortls.objects.fortran_submodule":[[2,5,1,"","get_ancestors"],[2,5,1,"","get_desc"],[2,5,1,"","get_type"],[2,5,1,"","require_inherit"],[2,5,1,"","require_link"],[2,5,1,"","resolve_inherit"],[2,5,1,"","resolve_link"]],"fortls.objects.fortran_subroutine":[[2,5,1,"","check_valid_parent"],[2,5,1,"","copy_interface"],[2,5,1,"","get_children"],[2,5,1,"","get_desc"],[2,5,1,"","get_diagnostics"],[2,5,1,"","get_docs_full"],[2,5,1,"","get_hover"],[2,5,1,"","get_interface"],[2,5,1,"","get_interface_array"],[2,5,1,"","get_signature"],[2,5,1,"","get_snippet"],[2,5,1,"","get_type"],[2,5,1,"","is_callable"],[2,5,1,"","is_mod_scope"],[2,5,1,"","require_link"],[2,5,1,"","resolve_arg_link"],[2,5,1,"","resolve_link"]],"fortls.objects.fortran_type":[[2,5,1,"","check_valid_parent"],[2,5,1,"","get_actions"],[2,5,1,"","get_children"],[2,5,1,"","get_desc"],[2,5,1,"","get_diagnostics"],[2,5,1,"","get_overridden"],[2,5,1,"","get_type"],[2,5,1,"","require_inherit"],[2,5,1,"","resolve_inherit"]],"fortls.objects.fortran_var":[[2,5,1,"","check_definition"],[2,5,1,"","get_desc"],[2,5,1,"","get_hover"],[2,5,1,"","get_snippet"],[2,5,1,"","get_type"],[2,5,1,"","get_type_obj"],[2,5,1,"","is_callable"],[2,5,1,"","is_optional"],[2,5,1,"","is_parameter"],[2,5,1,"","require_link"],[2,5,1,"","resolve_link"],[2,5,1,"","set_dim"],[2,5,1,"","set_external_attr"],[2,5,1,"","set_parameter_val"],[2,5,1,"","update_fqsn"]],"fortls.objects.fortran_where":[[2,5,1,"","get_desc"],[2,5,1,"","get_type"]],"fortls.parse_fortran":[[2,4,1,"","find_external"],[2,4,1,"","find_external_attr"],[2,4,1,"","find_external_type"],[2,2,1,"","fortran_file"],[2,4,1,"","get_line_context"],[2,4,1,"","get_procedure_modifiers"],[2,4,1,"","parse_var_keywords"],[2,4,1,"","preprocess_file"],[2,4,1,"","read_associate_def"],[2,4,1,"","read_block_def"],[2,4,1,"","read_do_def"],[2,4,1,"","read_enum_def"],[2,4,1,"","read_fun_def"],[2,4,1,"","read_generic_def"],[2,4,1,"","read_if_def"],[2,4,1,"","read_imp_stmt"],[2,4,1,"","read_inc_stmt"],[2,4,1,"","read_int_def"],[2,4,1,"","read_mod_def"],[2,4,1,"","read_prog_def"],[2,4,1,"","read_select_def"],[2,4,1,"","read_sub_def"],[2,4,1,"","read_submod_def"],[2,4,1,"","read_type_def"],[2,4,1,"","read_use_stmt"],[2,4,1,"","read_var_def"],[2,4,1,"","read_vis_stmnt"],[2,4,1,"","read_where_def"]],"fortls.parse_fortran.fortran_file":[[2,5,1,"","apply_change"],[2,5,1,"","check_file"],[2,5,1,"","copy"],[2,5,1,"","find_word_in_code_line"],[2,5,1,"","get_code_line"],[2,5,1,"","get_comment_regexs"],[2,5,1,"","get_docstring"],[2,5,1,"","get_fortran_definition"],[2,5,1,"","get_line"],[2,5,1,"","get_single_line_docstring"],[2,5,1,"","load_from_disk"],[2,5,1,"","parse"],[2,5,1,"","parse_contains"],[2,5,1,"","parse_do_fixed_format"],[2,5,1,"","parse_docs"],[2,5,1,"","parse_end_scope_word"],[2,5,1,"","parse_implicit"],[2,5,1,"","preprocess"],[2,5,1,"","set_contents"],[2,5,1,"","strip_comment"]],"fortls.regex_patterns":[[2,2,1,"","FortranRegularExpressions"],[2,4,1,"","src_file_exts"]],"fortls.regex_patterns.FortranRegularExpressions":[[2,3,1,"","ASSOCIATE"],[2,3,1,"","BLOCK"],[2,3,1,"","CALL"],[2,3,1,"","CLASS_VAR"],[2,3,1,"","CONTAINS"],[2,3,1,"","DEFINED"],[2,3,1,"","DEF_KIND"],[2,3,1,"","DO"],[2,3,1,"","DQ_STRING"],[2,3,1,"","END"],[2,3,1,"","END_ASSOCIATE"],[2,3,1,"","END_BLOCK"],[2,3,1,"","END_DO"],[2,3,1,"","END_ENUMD"],[2,3,1,"","END_FIXED"],[2,3,1,"","END_FUN"],[2,3,1,"","END_IF"],[2,3,1,"","END_INT"],[2,3,1,"","END_MOD"],[2,3,1,"","END_PRO"],[2,3,1,"","END_PROG"],[2,3,1,"","END_SELECT"],[2,3,1,"","END_SMOD"],[2,3,1,"","END_SUB"],[2,3,1,"","END_TYPED"],[2,3,1,"","END_WHERE"],[2,3,1,"","END_WORD"],[2,3,1,"","ENUM_DEF"],[2,3,1,"","EXTENDS"],[2,3,1,"","FIXED_COMMENT"],[2,3,1,"","FIXED_CONT"],[2,3,1,"","FIXED_DOC"],[2,3,1,"","FIXED_OPENMP"],[2,3,1,"","FREE_COMMENT"],[2,3,1,"","FREE_CONT"],[2,3,1,"","FREE_DOC"],[2,3,1,"","FREE_FORMAT_TEST"],[2,3,1,"","FREE_OPENMP"],[2,3,1,"","FUN"],[2,3,1,"","GENERIC_PRO"],[2,3,1,"","GEN_ASSIGN"],[2,3,1,"","IF"],[2,3,1,"","IMPLICIT"],[2,3,1,"","IMPORT"],[2,3,1,"","INCLUDE"],[2,3,1,"","INT"],[2,3,1,"","INT_STMNT"],[2,3,1,"","KEYWORD_LIST"],[2,3,1,"","KIND_SPEC"],[2,3,1,"","LINE_LABEL"],[2,3,1,"","LOGICAL"],[2,3,1,"","MOD"],[2,3,1,"","NON_DEF"],[2,3,1,"","NUMBER"],[2,3,1,"","OBJBREAK"],[2,3,1,"","PARAMETER_VAL"],[2,3,1,"","PP_ANY"],[2,3,1,"","PP_DEF"],[2,3,1,"","PP_DEF_TEST"],[2,3,1,"","PP_INCLUDE"],[2,3,1,"","PP_REGEX"],[2,3,1,"","PROCEDURE_STMNT"],[2,3,1,"","PROG"],[2,3,1,"","PRO_LINK"],[2,3,1,"","RESULT"],[2,3,1,"","SCOPE_DEF"],[2,3,1,"","SELECT"],[2,3,1,"","SELECT_DEFAULT"],[2,3,1,"","SELECT_TYPE"],[2,3,1,"","SQ_STRING"],[2,3,1,"","SUB"],[2,3,1,"","SUBMOD"],[2,3,1,"","SUB_MOD"],[2,3,1,"","SUB_PAREN"],[2,3,1,"","TATTR_LIST"],[2,3,1,"","THEN"],[2,3,1,"","TYPE_DEF"],[2,3,1,"","TYPE_STMNT"],[2,3,1,"","USE"],[2,3,1,"","VAR"],[2,3,1,"","VIS"],[2,3,1,"","WHERE"],[2,3,1,"","WORD"]],fortls:[[2,0,0,"-","constants"],[2,0,0,"-","ftypes"],[2,0,0,"-","helper_functions"],[2,0,0,"-","interface"],[2,0,0,"-","intrinsics"],[2,0,0,"-","json_templates"],[2,0,0,"-","jsonrpc"],[2,0,0,"-","langserver"],[2,0,0,"-","objects"],[2,0,0,"-","parse_fortran"],[2,0,0,"-","regex_patterns"],[2,0,0,"-","version"]]},objnames:{"0":["py","module","Python module"],"1":["py","data","Python data"],"2":["py","class","Python class"],"3":["py","attribute","Python attribute"],"4":["py","function","Python function"],"5":["py","method","Python method"],"6":["py","exception","Python exception"]},objtypes:{"0":"py:module","1":"py:data","2":"py:class","3":"py:attribute","4":"py:function","5":"py:method","6":"py:exception"},terms:{"0":[0,2,3],"1":[0,2,3,6],"10":[2,6],"11":0,"116":3,"12":2,"13":3,"14":[2,3],"16":3,"169":3,"17":[2,3],"18":3,"184":3,"187":3,"188":3,"19":2,"191":3,"2":[0,2],"200":3,"2017":4,"203":3,"206":3,"207":3,"22":[2,3],"3":[0,2,3],"32601":2,"33":3,"34":3,"35":3,"36":3,"39":3,"4":[2,3,6],"46":3,"47":3,"48":3,"5":2,"50":3,"51":3,"54":3,"55":3,"6":2,"60":3,"63":3,"67":3,"7":[0,3],"76":3,"78":3,"8":3,"80":3,"9":[2,3],"9_":2,"abstract":2,"boolean":2,"case":[2,6],"class":[0,2,3],"const":2,"default":[1,2,3,6],"do":[0,2,6],"enum":[2,3],"export":2,"final":[1,2],"function":[0,2,3,6],"goto":[0,3],"import":[0,2,3],"int":2,"long":2,"new":[0,2],"public":2,"return":[2,3],"short":2,"static":2,"true":[0,1,2],"try":[0,2],"var":2,"while":3,A:[2,6],By:6,FOR:[2,6],For:[0,1,6],IF:2,IN:2,IS:2,If:[0,1,2,6],In:2,It:[2,3],Or:1,THEN:2,The:[0,1,2,3,6],Then:1,To:[2,3,6],_:2,__literal_internal_dummy_var_:2,_hdf5:6,_skip:0,_tmp:6,about:2,absolut:[2,3],access:0,acknowledg:4,across:[0,2,3],action:[0,1,2,3,6],ad:[1,4],add:[1,2,3],add_child:2,add_doc:2,add_error:2,add_includ:2,add_int_memb:2,add_memb:2,add_priv:2,add_publ:2,add_rel:2,add_scop:2,add_subroutin:2,add_us:2,add_vari:2,addit:[2,3,6],adjac:2,after:2,against:2,alia:2,all:[0,1,2,6],allocat:2,allow:[0,1,3,6],allow_empti:2,along:2,alreadi:2,also:[2,6],altern:6,alwai:[2,3,6],ambigu:2,an:[0,1,2,3],anaconda:3,ancestor_nam:2,ani:[1,2],anoth:2,append:[2,6],appli:2,apply_chang:2,ar:[0,1,2,3,6],arbitrari:3,arg1:2,arg2:2,arg:[1,2],arg_list:2,arg_list_nam:2,arg_modifi:2,argpars:2,argument:[0,1,2,3],argumentpars:2,arrai:[2,3],assign:2,associ:[2,3],assum:6,ast:2,atom:[0,4],attempt:2,attribut:3,auto:[0,3],autocomplet:3,autocomplete_name_onli:6,autocomplete_no_prefix:[1,6],autocomplete_no_snippet:6,autogener:3,autom:3,automat:6,autoupd:[3,6],avail:[0,1,2],b:2,backward:2,bar:2,base:[0,2],been:[0,1,2,3],befor:2,behaviour:6,being:[2,3,6],below:[0,6],beta:6,between:[0,2],binari:0,bind:2,block:[0,2],block_id_stack:2,bodi:3,bool:2,both:[0,2],bound:[0,3],bound_nam:2,bracket:2,branch:3,buffer:1,bug:[3,4],build:2,c:[2,6],call:2,can:[0,1,2,3,6],cannot:[2,3,6],capabl:3,case_typ:2,caus:2,cd:2,certain:6,cfg:3,chang:[0,2,4],change_arg:2,change_json:2,change_str:2,channel:[2,3],char_po:2,charact:[2,6],check:[2,6],check_definit:2,check_fil:2,check_us:2,check_valid_par:2,child:2,choic:2,chosen:[0,2],ci:3,class_info:2,class_var:2,clearer:3,client:[1,2,3],climb_type_tre:2,close:[0,2],close_fil:2,code:[0,2,3,4,6],codeact:0,codecov:3,col:2,column:[2,3],command:[0,1,2,3,4],commandline_arg:2,comment:[2,3,6],compil:2,complet:[0,2,3,6],complex:2,compliant:3,compon:2,conda:3,condens:6,condit:2,config:[1,3,6],configur:[0,1,3,4],conn:2,connect:2,consecut:2,consid:6,consist:6,constant:[3,5],construct:2,contain:[0,2,6],container_nam:2,content:5,contents_split:2,context:2,contigu:2,continu:[2,3],control:1,convert:2,copi:2,copy_from:2,copy_interfac:2,copy_sourc:2,correspond:2,coverag:3,creat:[1,2],create_binding_vari:2,create_none_scop:2,curr_lin:2,curr_path:2,curr_scop:2,current:[2,6],cursor:[0,2],custom:1,d:2,data:2,dataclass:2,date:3,debug:[2,4],debug_act:6,debug_char:6,debug_complet:6,debug_definit:6,debug_diagnost:6,debug_filepath:6,debug_full_result:6,debug_hov:6,debug_implement:6,debug_lin:6,debug_log:6,debug_pars:6,debug_refer:6,debug_renam:6,debug_rootpath:6,debug_signatur:6,debug_symbol:6,debug_workspace_symbol:6,debugg:3,declar:2,def_char:2,def_fil:2,def_kind:2,def_lin:2,def_obj:2,defer:[0,2],deffin:6,defin:[0,1,2,3,6],definit:[0,1,2,3,6],depend:3,deprec:[1,4,6],deque_find_and_pop:2,desc:2,desc_str:2,descript:[0,2],dest:2,detail:[0,1,6],detect:[0,2,6],detect_fixed_format:2,detect_format:2,determin:2,dev:3,develop:[0,4],diagnost:[0,2,3],diagnostic_json:2,dict:2,dictionari:[2,6],did_clos:2,did_open:2,didchang:0,didclos:0,didopen:0,didsav:0,differ:[0,2,6],dim_str:2,dimens:2,direct:3,directori:[2,6],disabl:[2,3,6],disable_autoupd:[3,6],disable_diagnost:6,discuss:6,disk:2,displai:[2,3,6],diverg:0,doc:2,doc_str:2,docstr:2,document:[0,1,2,3,4,6],documentsymbol:[0,6],doe:[0,2],doubl:2,doxygen:[0,2],dparkin:1,dq_string:2,drop_arg:2,dure:[2,3,6],e:[0,2,3,6],each:1,ech:2,edit:1,editor:[0,3,4],effect:6,either:[1,2],elegantli:0,element:2,elif:2,elin:2,eln:2,els:2,emac:[0,4],empti:2,enabl:[1,2,3,6],enable_code_act:6,enc_scop:2,enclos:2,encod:3,encount:2,end:[2,3],end_associ:2,end_block:2,end_do:2,end_enumd:2,end_fix:2,end_fun:2,end_if:2,end_int:2,end_mod:2,end_ppif:2,end_pro:2,end_prog:2,end_scop:2,end_scope_regex:2,end_select:2,end_smod:2,end_sub:2,end_typ:2,end_wher:2,end_word:2,endif:2,entir:[2,6],enum_def:2,environ:3,err_msg:2,error:[0,2,3],etc:[0,2,6],eval:1,everyth:2,exact_match:2,exampl:[0,2],exc_info:2,except:2,excl_path:[0,3],excl_suffix:0,exclud:[3,6],exclude_dir:6,execut:[0,1],exist:[0,2],exit:6,expand:2,expand_nam:2,experiment:[0,6],express:2,extend:2,extens:[1,2,6],extern:[2,3,6],external_obj:2,extract:2,f03:[2,6],f05:[2,6],f08:[2,6],f18:[2,6],f2018:3,f2:1,f5:1,f77:[2,6],f90:[0,2,6],f95:[2,6],f:[2,3,6],factori:2,fals:[2,6],featur:[1,4,6],field:2,field_nam:2,file:[0,1,2,3,4],file_ast:2,file_init:2,file_lin:2,file_obj:2,file_path:2,filepath:[2,6],filetyp:1,filter:6,filter_publ:2,find:[0,2],find_extern:2,find_external_attr:2,find_external_typ:2,find_in_scop:2,find_in_workspac:2,find_paren_match:2,find_word:2,find_word_in_code_lin:2,find_word_in_lin:2,first:2,firstli:1,fix:[0,2,4],fixed_com:2,fixed_cont:2,fixed_doc:2,fixed_openmp:2,fixedform:1,flag:2,fnmatch:[2,6],folder:6,follow:[1,2,6],foo:2,foral:[2,3],ford:[0,2],forg:3,form:2,format:[2,3],fortl:[1,5,6],fortran90:6,fortran:[1,2,6],fortran_associ:2,fortran_ast:2,fortran_block:2,fortran_diagnost:2,fortran_do:2,fortran_enum:2,fortran_fil:2,fortran_funct:2,fortran_if:2,fortran_includ:2,fortran_int:2,fortran_intrinsic_obj:2,fortran_liter:2,fortran_meth:2,fortran_modul:2,fortran_obj:2,fortran_program:2,fortran_scop:2,fortran_select:2,fortran_submodul:2,fortran_subroutin:2,fortran_typ:2,fortran_var:2,fortran_wher:2,fortranregularexpress:2,forward:2,found:[0,2,6],fpp:[2,6],fqsn:2,free:2,free_com:2,free_cont:2,free_doc:2,free_format_test:2,free_openmp:2,from:[0,1,2,3,6],ftype:5,full:[0,2,6],fun:2,fun_onli:2,fun_sig:2,further:3,fyp:6,g:[0,1,2,6],gcc:0,gd:1,gen_assign:2,gen_info:2,gener:[0,2,6],generic_pro:2,get:[0,1,2],get_act:2,get_all_refer:2,get_ancestor:2,get_children:2,get_code_lin:2,get_comment_regex:2,get_definit:2,get_desc:2,get_diagnost:2,get_docs_ful:2,get_docstr:2,get_document:2,get_enc_scope_nam:2,get_fortran_definit:2,get_hov:2,get_implicit:2,get_inner_scop:2,get_interfac:2,get_interface_arrai:2,get_intrinsic_keyword:2,get_keyword:2,get_lin:2,get_line_context:2,get_line_prefix:2,get_object:2,get_overridden:2,get_paren_level:2,get_paren_substr:2,get_placehold:2,get_procedure_modifi:2,get_scop:2,get_signatur:2,get_single_line_docstr:2,get_snippet:2,get_typ:2,get_type_obj:2,get_use_tre:2,get_var_stack:2,git:1,github:[3,6],given:[0,2],glob:[2,3,6],glob_path:2,global:[1,3],gnikit:[1,3,6],go:[0,3],group:2,gvim:4,h:[0,6],ha:[0,1,2,3],handl:[0,2,3],hansec:[0,1],hash:2,have:[0,1,2,6],have_hdf5:0,have_petsc:6,help:[0,2,6],helper_funct:5,henc:1,here:[0,2],hidden:1,highlight:1,hold:2,holder:6,hook:1,host:3,hover:[0,1,2,3],hover_arrai:2,hover_languag:6,hover_req:2,hover_signatur:[0,1,6],how:3,howev:[2,6],html:[2,6],http:[2,6],i:[0,2,3],id:[1,2,6],ident:[0,6],ieee_arithmet:0,ieee_except:0,ieee_featur:0,ifdef:2,ifndef:2,ignor:2,ignorecas:2,implement:[3,4,6],implicit:[0,2],implicit_flag:2,implicitli:3,improv:3,impur:2,in_lin:2,incl_suffix:3,includ:[0,1,2,3,6],include_dir:[0,2,3],include_doc:2,include_info:2,incomplet:2,increment:6,incremental_sync:[0,1,6],independ:0,index:[2,4,6],indic:2,infer:2,info:2,inform:[0,2,6],inherit_typ:2,inherit_vers:2,init:2,init_var:2,initi:6,initialis:[2,3],inout:2,input:2,input_ext:2,insid:1,instal:[1,4,6],instantli:1,instead:[3,6],int_info:2,int_onli:2,int_stmnt:2,integ:[0,2,6],integr:[0,3,4],intent:2,interchang:6,interfac:[0,1,3,5,6],interface_str:2,intermingl:3,interrupt:2,intersect:2,intrins:[0,3,5,6],invalid:[0,2],io:[2,6],ios_c_bind:0,is_abstract:2,is_cal:2,is_external_int:2,is_mod_scop:2,is_opt:2,is_paramet:2,is_type_bind:2,is_type_region:2,iso_fortran_env:0,isort:3,issu:[0,3],item:2,its:[2,3,6],itself:2,json:[0,2,3,6],json_templ:5,jsonrpc2connect:2,jsonrpc2error:2,jsonrpc2protocolerror:2,jsonrpc:5,k:1,kak:1,kak_sess:1,kakoun:4,kakrc:1,kei:2,keyword:[2,3,6],keyword_info:2,keyword_list:2,kind:2,kind_spec:2,know:2,known_typ:2,label:2,langserv:5,languag:[1,2,6],languagecli:1,languageclient_servercommand:1,lcn:1,lead:[2,6],leak:2,left:2,legaci:2,len:2,length:[3,6],let:1,level:2,librari:2,licens:4,like:[1,6],line:[0,2,3,4],line_label:2,line_no:2,line_numb:2,link:2,link_obj:2,link_var:2,link_vers:2,list:[0,2,3],liter:[2,3],ln:2,load_from_disk:2,load_intrins:2,local_onli:2,locat:6,location_json:2,log:[2,3,6],logic:2,loglevel:3,look:[2,6],loop:2,lower:6,lowercas:6,lowercase_intrins:[0,6],lowercase_intris:1,lsp:[0,1],made:0,maintain:2,maintain_len:2,make:1,manag:2,manner:6,map:1,map_keyword:2,mark:[2,3],mark_contain:2,mask:0,master:3,mat:6,match:[2,3],max:3,max_comment_line_length:[2,6],max_line_length:[2,6],maximum:6,md5:2,md:3,member:[2,6],menu:1,messag:[0,2,3,6],metavar:2,method:[2,3],metric:3,might:2,miss:[0,3],mit:0,mod:2,mod_flag:2,mod_mem:2,mod_nam:2,mod_onli:2,mod_word:2,mode:1,modern:[1,2],modifi:[1,2,3],modul:[0,3,4,5],modular:3,more:[1,3,6],mpi_comm:0,msg:2,multilin:[2,3],multipl:[0,1,2,3],must:2,mutabl:3,my_project:6,myarrai:2,myvar:2,n:6,name:[0,2,3],name_replac:2,name_strip:2,narg:2,nativ:1,natur:2,necessarili:2,need:[2,6],neo:0,neovim:4,nest:0,new_scop:2,new_text:2,new_var:2,new_vi:2,newer:6,next:2,nmap:1,no_contain:2,no_link:2,node:2,non:[0,2],non_def:2,non_intrins:[2,3],none:2,nopass:2,noremap:1,normal:[2,6],note:[1,2],notif:[2,6],notify_init:[1,6],now:[0,1,3,6],nth:2,nthread:6,number:[2,6],numer:[0,2],obj_nam:2,obj_tre:2,objbreak:2,object:[0,3,5,6],observ:0,occur:2,older:1,omp:2,omp_lib:[0,3],omp_lib_kind:[0,3],one:6,ones:6,onli:[0,2,4],only_dir:2,only_list:2,open:[0,2],openacc:[0,3],openacc_kind:0,openmp:0,oper:[1,2],opposit:3,option:[0,1,2,3,4],option_str:2,order:3,org:2,origin:0,other:6,otherwis:2,our:6,out:2,outermost:2,output:2,over:[2,3,6],overload:0,overrid:[3,6],overriden:6,overwritten:6,packag:[1,3,4,5],page:4,pair:2,param:2,paramet:[2,3,6],parameter_v:2,parent:[0,2,3],parent_obj:2,parenthesi:[2,3],pars:[0,2,3],parse_contain:2,parse_do_fixed_format:2,parse_doc:2,parse_end_scope_word:2,parse_fortran:5,parse_implicit:2,parse_var_keyword:2,parser:[2,3,6],part:2,partial:3,pass:2,path:[1,2,3,6],path_from_uri:2,path_to_uri:2,pattern:[2,6],peek:0,pend:2,pip:[0,3],pipelin:2,place:6,placement:0,pleas:0,plug:1,plugin:[0,1],pointer:[2,3],posit:[0,2,6],possibl:[0,2],post_messag:2,potenti:[0,2],pp_ani:2,pp_content:2,pp_def:[0,2,3],pp_def_test:2,pp_includ:2,pp_regex:2,pp_suffix:[0,2],pre:[0,3],pre_lin:2,preced:[2,6],precis:2,prefix:[2,6],prematur:3,prepar:3,preproc:2,preprocess:2,preprocess_fil:2,preprocessor:[0,2,3],present:2,preserve_keyword_ord:[3,6],prettier:3,previou:2,print:6,privat:[2,3],pro_lin:2,pro_link:2,procedur:[0,2,3,6],procedure_stmnt:2,process:2,produc:2,prog:2,program:2,project:[0,6],propag:3,protocol:0,provid:0,public_onli:2,pure:2,py:3,pypi:[3,6],pyproject:3,python:[0,2,3],qs:2,queri:2,query_str:6,quot:2,rais:2,rang:2,range_json:2,rank:2,re:2,reachabl:1,read:[2,3],read_associate_def:2,read_block_def:2,read_do_def:2,read_enum_def:2,read_fil:2,read_fun_def:2,read_generic_def:2,read_if_def:2,read_imp_stmt:2,read_inc_stmt:2,read_int_def:2,read_messag:2,read_mod_def:2,read_prog_def:2,read_rpc_messag:2,read_select_def:2,read_sub_def:2,read_submod_def:2,read_type_def:2,read_use_stmt:2,read_var_def:2,read_vis_stmnt:2,read_where_def:2,reader:2,readlin:2,readm:3,readwrit:2,real:2,reamd:3,recommend:0,recurs:[2,6],refer:[0,6],regex:2,regex_pattern:5,regular:2,reinstal:0,rel:2,releas:3,relev:2,remain:0,remov:6,renam:[0,1,2,3,6],rename_map:2,rename_str:6,report:[3,4],repres:[1,2],reproduc:0,req_contain:2,req_named_end:2,request:[2,4,6],requir:[1,2],require_inherit:2,require_link:2,resolv:2,resolve_arg_link:2,resolve_glob:2,resolve_includ:2,resolve_inherit:2,resolve_link:2,respons:[2,6],rest:2,result:[2,3,6],result_modifi:2,result_nam:2,result_sig:2,result_typ:2,result_var:2,rewrot:3,rid:2,root:[1,2,6],root_path:[2,6],rpc:2,run:2,s:[0,1,2,3,6],same:[0,2,3],save:[0,2],scaled_vector:2,scan:6,sch:2,scope:[0,2,3],scope_def:2,scope_obj:2,search:[2,4],section:[0,2,3,6],see:[0,1,2,6],select:2,select_default:2,select_info:2,select_typ:2,selector:1,self:2,semant:3,send:6,send_diagnost:2,send_notif:2,send_request:2,send_request_batch:2,sent:2,separ:[1,2,3],separate_def_list:2,seper:2,sequenc:2,seri:6,serve_autocomplet:2,serve_codeact:2,serve_default:2,serve_definit:2,serve_document_symbol:2,serve_exit:2,serve_hov:2,serve_implement:2,serve_initi:2,serve_onchang:2,serve_onclos:2,serve_onopen:2,serve_onsav:2,serve_refer:2,serve_renam:2,serve_signatur:2,serve_workspace_symbol:2,server:[1,2,6],set:[1,2,3,4,6],set_cont:2,set_default_vi:2,set_dim:2,set_external_attr:2,set_implicit:2,set_inherit:2,set_keyword_ord:2,set_lowercase_intrins:2,set_par:2,set_parameter_v:2,set_vis:2,setact:2,settrac:3,setup:3,setuptools_scm:3,sev:2,sever:2,sh:1,shield:3,should:[1,2,6],show:[0,3,6],sig:2,signatur:[0,2,3,6],signaturehelp:[0,6],silent:1,simplifi:3,simultan:0,sinc:[0,2,3],singl:2,size:3,skip:[2,3],slightli:2,sline:2,sln:2,smod_info:2,snippet:6,so:6,sole:[1,2],some:[0,2,3],sort:[2,6],sort_keyword:[3,6],sourc:[1,3],source_dir:3,space:3,specif:6,specifi:[1,6],sphinx:3,split:2,sq_string:2,src:6,src_file_ext:2,standardis:3,start:[2,6],start_ppif:2,state:3,statement:[0,2,3],sting:2,store:3,str:2,string:[2,3],strip:2,strip_com:2,strip_line_label:2,strip_str:2,studio:[0,4],style:[0,2,6],sub1:2,sub:2,sub_info:2,sub_mod:2,sub_paren:2,subdirectori:6,subject:0,sublim:[0,4],submit:3,submod:2,submodul:[3,5],subroutin:[0,2,6],substitut:[3,6],succe:2,suffix:6,suggest:0,support:[1,3,4],sure:1,surround:3,symbol:0,symbol_json:2,symbol_skip_mem:[1,6],synchron:6,synchronis:0,syntax:[1,2],tab:0,tag:2,target:2,tattr_list:2,tcpreadwrit:2,templat:0,termin:2,test:[0,6],test_str:2,text:[0,2,4],textdocu:[0,6],thei:[0,2,3,6],theire:3,them:6,thi:[0,1,2,6],thread:[2,6],through:[0,1,2,3,6],tmat:6,todo:2,toggl:2,toml:[1,3],tool:0,tradit:2,trail:2,treat:2,tree:2,trigger:3,troubl:0,tupl:[2,3],two:[0,2],type:[0,2,3,6],type_def:2,type_mem:2,type_onli:2,type_stmnt:2,unclos:0,undef:2,under:[0,6],unifi:3,unimpl:0,uninstal:0,uniqu:[0,4],unittest:3,unix:3,unknown:0,unlabel:2,unless:6,until:2,up:[2,3],updat:[2,3],update_fqsn:2,update_link:2,update_workspace_fil:2,upgrad:0,upon:[0,2,3],upper:6,uppercas:6,upstream:0,uri:2,uri_json:2,us:[0,1,2,3,4],usag:6,use_dict:2,use_info:[2,3],use_lin:2,use_mod:2,use_signature_help:[0,1,6],user:[0,2],usr:6,utf:3,v1:3,v3:0,v5:[0,3],v:6,val:2,valid:2,valu:[2,3],var1:[2,6],var2:[2,6],var3:2,var_desc:2,var_info:2,var_kei:2,var_line_numb:2,var_nam:2,var_onli:2,var_stack:2,var_typ:2,variabl:[0,1,2,3,6],variable_hov:[3,6],version:[3,5,6],vi:2,via:[2,6],vim:[0,4],vimrc:1,vis_flag:2,vis_info:2,visibl:[0,2],visual:[0,4],vs17:1,vs:[3,4],vscode:6,w:2,wa:[1,3],wai:[2,3],walk:2,want:[0,1,2,6],warn:[0,2,3],we:2,what:6,when:[0,2,3,6],where:[2,3],whether:2,which:[2,3],wide:0,window:[1,3],winsetopt:1,without:[0,2,3],witout:3,word:2,work:[0,3],workflow:[0,3],workspac:[0,2,6],workspace_init:2,would:0,write:2,write_error:2,write_respons:2,write_rpc_notif:2,write_rpc_request:2,writer:2,xor_eq:2,you:[0,1,2,6],your:1,z0:2,z:2,z_:2},titles:["fortls - Fortran Language Server","Editor Integration","fortls package","Unique fortls features (not in fortran-language-server)","fortls \u2013 Fortran Language Server","Developers\u2019 documentations","Configuration options"],titleterms:{"2017":1,acknowledg:0,ad:3,argument:6,atom:1,autocomplet:6,avail:6,bug:0,chang:3,code:1,codeact:6,command:6,configur:6,constant:2,content:[2,4],debug:6,deprec:3,develop:5,diagnost:6,document:5,editor:1,emac:1,error:6,excl_path:6,excl_suffix:6,featur:[0,3],file:6,fix:3,fortl:[0,2,3,4],fortran:[0,3,4],ftype:2,gvim:1,helper_funct:2,hover:6,implement:0,incl_suffix:6,include_dir:6,indic:4,instal:0,integr:1,interfac:2,intrins:2,json_templ:2,jsonrpc:2,kakoun:1,langserv:2,languag:[0,3,4],licens:0,limit:[0,6],line:6,modul:2,name:6,neovim:1,note:0,object:2,onli:6,option:6,packag:2,pars:6,parse_fortran:2,pp_def:6,pp_suffix:6,preprocessor:6,regex_pattern:2,report:0,request:0,server:[0,3,4],set:0,sourc:6,source_dir:6,studio:1,sublim:1,submodul:2,support:0,swigl:6,symbol:6,tabl:4,text:1,uniqu:3,us:6,version:2,vim:1,visual:1,vs:0}})