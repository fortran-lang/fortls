Search.setIndex({docnames:["contact","contributing","editor_integration","features","fortls","fortls_changes","index","modules","options","quickstart"],envversion:{"sphinx.domains.c":2,"sphinx.domains.changeset":1,"sphinx.domains.citation":1,"sphinx.domains.cpp":5,"sphinx.domains.index":1,"sphinx.domains.javascript":2,"sphinx.domains.math":2,"sphinx.domains.python":3,"sphinx.domains.rst":2,"sphinx.domains.std":2,"sphinx.ext.intersphinx":1,sphinx:56},filenames:["contact.rst","contributing.rst","editor_integration.rst","features.rst","fortls.rst","fortls_changes.md","index.rst","modules.rst","options.rst","quickstart.rst"],objects:{"":[[4,0,0,"-","fortls"]],"fortls.constants":[[4,1,1,"","FORTRAN_LITERAL"],[4,2,1,"","Severity"]],"fortls.constants.Severity":[[4,3,1,"","error"],[4,3,1,"","info"],[4,3,1,"","warn"]],"fortls.ftypes":[[4,2,1,"","ClassInfo"],[4,2,1,"","FunSig"],[4,2,1,"","GenProcDefInfo"],[4,2,1,"","IncludeInfo"],[4,2,1,"","InterInfo"],[4,2,1,"","Range"],[4,2,1,"","ResultSig"],[4,2,1,"","SelectInfo"],[4,2,1,"","SmodInfo"],[4,2,1,"","SubInfo"],[4,2,1,"","UseInfo"],[4,2,1,"","VarInfo"],[4,2,1,"","VisInfo"]],"fortls.ftypes.ClassInfo":[[4,3,1,"","keywords"],[4,3,1,"","name"],[4,3,1,"","parent"]],"fortls.ftypes.FunSig":[[4,3,1,"","result"]],"fortls.ftypes.GenProcDefInfo":[[4,3,1,"","bound_name"],[4,3,1,"","pro_links"],[4,3,1,"","vis_flag"]],"fortls.ftypes.IncludeInfo":[[4,3,1,"","file"],[4,3,1,"","line_number"],[4,3,1,"","path"],[4,3,1,"","scope_objs"]],"fortls.ftypes.InterInfo":[[4,3,1,"","abstract"],[4,3,1,"","name"]],"fortls.ftypes.Range":[[4,3,1,"","end"],[4,3,1,"","start"]],"fortls.ftypes.ResultSig":[[4,3,1,"","keywords"],[4,3,1,"","name"],[4,3,1,"","type"]],"fortls.ftypes.SelectInfo":[[4,3,1,"","binding"],[4,3,1,"","desc"],[4,3,1,"","type"]],"fortls.ftypes.SmodInfo":[[4,3,1,"","name"],[4,3,1,"","parent"]],"fortls.ftypes.SubInfo":[[4,3,1,"","args"],[4,3,1,"","keywords"],[4,3,1,"","mod_flag"],[4,3,1,"","name"]],"fortls.ftypes.UseInfo":[[4,3,1,"","mod_name"],[4,3,1,"","only_list"],[4,3,1,"","rename_map"]],"fortls.ftypes.VarInfo":[[4,3,1,"","keywords"],[4,3,1,"","var_names"],[4,3,1,"","var_type"]],"fortls.ftypes.VisInfo":[[4,3,1,"","obj_names"],[4,3,1,"","type"]],"fortls.helper_functions":[[4,4,1,"","detect_fixed_format"],[4,4,1,"","expand_name"],[4,4,1,"","find_paren_match"],[4,4,1,"","find_word_in_line"],[4,4,1,"","get_keywords"],[4,4,1,"","get_line_prefix"],[4,4,1,"","get_paren_level"],[4,4,1,"","get_paren_substring"],[4,4,1,"","get_var_stack"],[4,4,1,"","map_keywords"],[4,4,1,"","only_dirs"],[4,4,1,"","resolve_globs"],[4,4,1,"","separate_def_list"],[4,4,1,"","set_keyword_ordering"],[4,4,1,"","strip_line_label"],[4,4,1,"","strip_strings"]],"fortls.interface":[[4,2,1,"","SetAction"],[4,4,1,"","commandline_args"]],"fortls.intrinsics":[[4,2,1,"","Intrinsic"],[4,4,1,"","get_intrinsic_keywords"],[4,4,1,"","load_intrinsics"],[4,4,1,"","set_lowercase_intrinsics"]],"fortls.intrinsics.Intrinsic":[[4,5,1,"","get_desc"],[4,5,1,"","get_hover"],[4,5,1,"","get_signature"],[4,5,1,"","get_snippet"],[4,5,1,"","get_type"],[4,5,1,"","is_callable"]],"fortls.json_templates":[[4,4,1,"","change_json"],[4,4,1,"","diagnostic_json"],[4,4,1,"","location_json"],[4,4,1,"","range_json"],[4,4,1,"","symbol_json"],[4,4,1,"","uri_json"]],"fortls.jsonrpc":[[4,2,1,"","JSONRPC2Connection"],[4,6,1,"","JSONRPC2ProtocolError"],[4,2,1,"","ReadWriter"],[4,2,1,"","TCPReadWriter"],[4,4,1,"","deque_find_and_pop"],[4,4,1,"","path_from_uri"],[4,4,1,"","path_to_uri"],[4,4,1,"","read_rpc_messages"],[4,4,1,"","write_rpc_notification"],[4,4,1,"","write_rpc_request"]],"fortls.jsonrpc.JSONRPC2Connection":[[4,5,1,"","read_message"],[4,5,1,"","send_notification"],[4,5,1,"","send_request"],[4,5,1,"","send_request_batch"],[4,5,1,"","write_error"],[4,5,1,"","write_response"]],"fortls.jsonrpc.ReadWriter":[[4,5,1,"","read"],[4,5,1,"","readline"],[4,5,1,"","write"]],"fortls.jsonrpc.TCPReadWriter":[[4,5,1,"","read"],[4,5,1,"","readline"],[4,5,1,"","write"]],"fortls.langserver":[[4,6,1,"","JSONRPC2Error"],[4,2,1,"","LangServer"]],"fortls.langserver.LangServer":[[4,5,1,"","file_init"],[4,5,1,"","get_all_references"],[4,5,1,"","get_definition"],[4,5,1,"","get_diagnostics"],[4,5,1,"","handle"],[4,5,1,"","post_message"],[4,5,1,"","run"],[4,5,1,"","send_diagnostics"],[4,5,1,"","serve_autocomplete"],[4,5,1,"","serve_codeActions"],[4,5,1,"","serve_default"],[4,5,1,"","serve_definition"],[4,5,1,"","serve_document_symbols"],[4,5,1,"","serve_exit"],[4,5,1,"","serve_hover"],[4,5,1,"","serve_implementation"],[4,5,1,"","serve_initialize"],[4,5,1,"","serve_onChange"],[4,5,1,"","serve_onClose"],[4,5,1,"","serve_onOpen"],[4,5,1,"","serve_onSave"],[4,5,1,"","serve_references"],[4,5,1,"","serve_rename"],[4,5,1,"","serve_signature"],[4,5,1,"","serve_workspace_symbol"],[4,5,1,"","update_workspace_file"],[4,5,1,"","workspace_init"]],"fortls.objects":[[4,2,1,"","Associate"],[4,2,1,"","AssociateMap"],[4,2,1,"","Block"],[4,2,1,"","Diagnostic"],[4,2,1,"","Do"],[4,2,1,"","Enum"],[4,2,1,"","FortranAST"],[4,2,1,"","FortranObj"],[4,2,1,"","Function"],[4,2,1,"","If"],[4,2,1,"","Include"],[4,2,1,"","Interface"],[4,2,1,"","Method"],[4,2,1,"","Module"],[4,2,1,"","Program"],[4,2,1,"","Scope"],[4,2,1,"","Select"],[4,2,1,"","Submodule"],[4,2,1,"","Subroutine"],[4,2,1,"","Type"],[4,2,1,"","USE_line"],[4,2,1,"","Variable"],[4,2,1,"","Where"],[4,4,1,"","climb_type_tree"],[4,4,1,"","find_in_scope"],[4,4,1,"","find_in_workspace"],[4,4,1,"","get_use_tree"]],"fortls.objects.Associate":[[4,5,1,"","create_binding_variable"],[4,5,1,"","get_desc"],[4,5,1,"","get_type"],[4,5,1,"","require_link"],[4,5,1,"","resolve_link"]],"fortls.objects.AssociateMap":[[4,3,1,"","bind_name"],[4,3,1,"","link_name"],[4,3,1,"","var"]],"fortls.objects.Block":[[4,5,1,"","get_children"],[4,5,1,"","get_desc"],[4,5,1,"","get_type"],[4,5,1,"","req_named_end"]],"fortls.objects.Diagnostic":[[4,5,1,"","add_related"],[4,5,1,"","build"]],"fortls.objects.Do":[[4,5,1,"","get_desc"],[4,5,1,"","get_type"]],"fortls.objects.Enum":[[4,5,1,"","get_desc"],[4,5,1,"","get_type"]],"fortls.objects.FortranAST":[[4,5,1,"","add_doc"],[4,5,1,"","add_error"],[4,5,1,"","add_include"],[4,5,1,"","add_int_member"],[4,5,1,"","add_private"],[4,5,1,"","add_public"],[4,5,1,"","add_scope"],[4,5,1,"","add_use"],[4,5,1,"","add_variable"],[4,5,1,"","check_file"],[4,5,1,"","close_file"],[4,5,1,"","create_none_scope"],[4,5,1,"","end_ppif"],[4,5,1,"","end_scope"],[4,5,1,"","get_enc_scope_name"],[4,5,1,"","get_inner_scope"],[4,5,1,"","get_object"],[4,5,1,"","get_scopes"],[4,5,1,"","resolve_includes"],[4,5,1,"","resolve_links"],[4,5,1,"","start_ppif"]],"fortls.objects.FortranObj":[[4,5,1,"","add_doc"],[4,5,1,"","check_definition"],[4,5,1,"","check_valid_parent"],[4,5,1,"","end"],[4,5,1,"","get_actions"],[4,5,1,"","get_ancestors"],[4,5,1,"","get_children"],[4,5,1,"","get_desc"],[4,5,1,"","get_diagnostics"],[4,5,1,"","get_documentation"],[4,5,1,"","get_hover"],[4,5,1,"","get_implicit"],[4,5,1,"","get_interface"],[4,5,1,"","get_placeholders"],[4,5,1,"","get_signature"],[4,5,1,"","get_snippet"],[4,5,1,"","get_type"],[4,5,1,"","get_type_obj"],[4,5,1,"","is_abstract"],[4,5,1,"","is_callable"],[4,5,1,"","is_external_int"],[4,5,1,"","is_mod_scope"],[4,5,1,"","is_optional"],[4,5,1,"","req_named_end"],[4,5,1,"","require_inherit"],[4,5,1,"","require_link"],[4,5,1,"","resolve_inherit"],[4,5,1,"","resolve_link"],[4,5,1,"","set_default_vis"],[4,5,1,"","set_parent"],[4,5,1,"","set_visibility"],[4,5,1,"","update_fqsn"]],"fortls.objects.Function":[[4,5,1,"","copy_interface"],[4,5,1,"","get_desc"],[4,5,1,"","get_hover"],[4,5,1,"","get_interface"],[4,5,1,"","get_type"],[4,5,1,"","is_callable"],[4,5,1,"","resolve_link"]],"fortls.objects.If":[[4,5,1,"","get_desc"],[4,5,1,"","get_type"]],"fortls.objects.Include":[[4,5,1,"","get_desc"]],"fortls.objects.Interface":[[4,5,1,"","get_desc"],[4,5,1,"","get_type"],[4,5,1,"","is_abstract"],[4,5,1,"","is_callable"],[4,5,1,"","is_external_int"],[4,5,1,"","require_link"],[4,5,1,"","resolve_link"]],"fortls.objects.Method":[[4,5,1,"","check_definition"],[4,5,1,"","get_documentation"],[4,5,1,"","get_hover"],[4,5,1,"","get_interface"],[4,5,1,"","get_signature"],[4,5,1,"","get_snippet"],[4,5,1,"","get_type"],[4,5,1,"","is_callable"],[4,5,1,"","resolve_link"],[4,5,1,"","set_parent"]],"fortls.objects.Module":[[4,5,1,"","check_valid_parent"],[4,5,1,"","get_desc"],[4,5,1,"","get_type"]],"fortls.objects.Program":[[4,5,1,"","get_desc"]],"fortls.objects.Scope":[[4,5,1,"","add_child"],[4,5,1,"","add_member"],[4,5,1,"","add_subroutine"],[4,5,1,"","add_use"],[4,5,1,"","check_definitions"],[4,5,1,"","check_use"],[4,5,1,"","copy_from"],[4,5,1,"","get_children"],[4,5,1,"","mark_contains"],[4,5,1,"","set_implicit"],[4,5,1,"","set_inherit"],[4,5,1,"","set_parent"],[4,5,1,"","update_fqsn"]],"fortls.objects.Select":[[4,5,1,"","create_binding_variable"],[4,5,1,"","get_desc"],[4,5,1,"","get_type"],[4,5,1,"","is_type_binding"],[4,5,1,"","is_type_region"]],"fortls.objects.Submodule":[[4,5,1,"","get_ancestors"],[4,5,1,"","get_desc"],[4,5,1,"","get_type"],[4,5,1,"","require_inherit"],[4,5,1,"","require_link"],[4,5,1,"","resolve_inherit"],[4,5,1,"","resolve_link"]],"fortls.objects.Subroutine":[[4,5,1,"","check_valid_parent"],[4,5,1,"","copy_interface"],[4,5,1,"","get_children"],[4,5,1,"","get_desc"],[4,5,1,"","get_diagnostics"],[4,5,1,"","get_docs_full"],[4,5,1,"","get_hover"],[4,5,1,"","get_interface"],[4,5,1,"","get_interface_array"],[4,5,1,"","get_signature"],[4,5,1,"","get_snippet"],[4,5,1,"","get_type"],[4,5,1,"","is_callable"],[4,5,1,"","is_mod_scope"],[4,5,1,"","require_link"],[4,5,1,"","resolve_arg_link"],[4,5,1,"","resolve_link"]],"fortls.objects.Type":[[4,5,1,"","check_valid_parent"],[4,5,1,"","get_actions"],[4,5,1,"","get_children"],[4,5,1,"","get_desc"],[4,5,1,"","get_diagnostics"],[4,5,1,"","get_overridden"],[4,5,1,"","get_type"],[4,5,1,"","require_inherit"],[4,5,1,"","resolve_inherit"]],"fortls.objects.Variable":[[4,5,1,"","check_definition"],[4,5,1,"","get_desc"],[4,5,1,"","get_hover"],[4,5,1,"","get_keywords"],[4,5,1,"","get_snippet"],[4,5,1,"","get_type"],[4,5,1,"","get_type_obj"],[4,5,1,"","is_callable"],[4,5,1,"","is_optional"],[4,5,1,"","is_parameter"],[4,5,1,"","require_link"],[4,5,1,"","resolve_link"],[4,5,1,"","set_dim"],[4,5,1,"","set_external_attr"],[4,5,1,"","set_parameter_val"],[4,5,1,"","update_fqsn"]],"fortls.objects.Where":[[4,5,1,"","get_desc"],[4,5,1,"","get_type"]],"fortls.parse_fortran":[[4,2,1,"","FortranFile"],[4,4,1,"","find_external"],[4,4,1,"","find_external_attr"],[4,4,1,"","find_external_type"],[4,4,1,"","get_line_context"],[4,4,1,"","get_procedure_modifiers"],[4,4,1,"","parse_var_keywords"],[4,4,1,"","preprocess_file"],[4,4,1,"","read_associate_def"],[4,4,1,"","read_block_def"],[4,4,1,"","read_do_def"],[4,4,1,"","read_enum_def"],[4,4,1,"","read_fun_def"],[4,4,1,"","read_generic_def"],[4,4,1,"","read_if_def"],[4,4,1,"","read_imp_stmt"],[4,4,1,"","read_inc_stmt"],[4,4,1,"","read_int_def"],[4,4,1,"","read_mod_def"],[4,4,1,"","read_prog_def"],[4,4,1,"","read_select_def"],[4,4,1,"","read_sub_def"],[4,4,1,"","read_submod_def"],[4,4,1,"","read_type_def"],[4,4,1,"","read_use_stmt"],[4,4,1,"","read_var_def"],[4,4,1,"","read_vis_stmnt"],[4,4,1,"","read_where_def"]],"fortls.parse_fortran.FortranFile":[[4,5,1,"","apply_change"],[4,5,1,"","check_file"],[4,5,1,"","copy"],[4,5,1,"","find_word_in_code_line"],[4,5,1,"","get_code_line"],[4,5,1,"","get_comment_regexs"],[4,5,1,"","get_docstring"],[4,5,1,"","get_fortran_definition"],[4,5,1,"","get_line"],[4,5,1,"","get_single_line_docstring"],[4,5,1,"","load_from_disk"],[4,5,1,"","parse"],[4,5,1,"","parse_contains"],[4,5,1,"","parse_do_fixed_format"],[4,5,1,"","parse_docs"],[4,5,1,"","parse_end_scope_word"],[4,5,1,"","parse_implicit"],[4,5,1,"","preprocess"],[4,5,1,"","set_contents"],[4,5,1,"","strip_comment"]],"fortls.regex_patterns":[[4,2,1,"","FortranRegularExpressions"],[4,4,1,"","src_file_exts"]],"fortls.regex_patterns.FortranRegularExpressions":[[4,3,1,"","ASSOCIATE"],[4,3,1,"","BLOCK"],[4,3,1,"","CALL"],[4,3,1,"","CLASS_VAR"],[4,3,1,"","CONTAINS"],[4,3,1,"","DEFINED"],[4,3,1,"","DEF_KIND"],[4,3,1,"","DO"],[4,3,1,"","DQ_STRING"],[4,3,1,"","END"],[4,3,1,"","END_ASSOCIATE"],[4,3,1,"","END_BLOCK"],[4,3,1,"","END_DO"],[4,3,1,"","END_ENUMD"],[4,3,1,"","END_FIXED"],[4,3,1,"","END_FUN"],[4,3,1,"","END_IF"],[4,3,1,"","END_INT"],[4,3,1,"","END_MOD"],[4,3,1,"","END_PRO"],[4,3,1,"","END_PROG"],[4,3,1,"","END_SELECT"],[4,3,1,"","END_SMOD"],[4,3,1,"","END_SUB"],[4,3,1,"","END_TYPED"],[4,3,1,"","END_WHERE"],[4,3,1,"","END_WORD"],[4,3,1,"","ENUM_DEF"],[4,3,1,"","EXTENDS"],[4,3,1,"","FIXED_COMMENT"],[4,3,1,"","FIXED_CONT"],[4,3,1,"","FIXED_DOC"],[4,3,1,"","FIXED_OPENMP"],[4,3,1,"","FREE_COMMENT"],[4,3,1,"","FREE_CONT"],[4,3,1,"","FREE_DOC"],[4,3,1,"","FREE_FORMAT_TEST"],[4,3,1,"","FREE_OPENMP"],[4,3,1,"","FUN"],[4,3,1,"","GENERIC_PRO"],[4,3,1,"","GEN_ASSIGN"],[4,3,1,"","IF"],[4,3,1,"","IMPLICIT"],[4,3,1,"","IMPORT"],[4,3,1,"","INCLUDE"],[4,3,1,"","INT"],[4,3,1,"","INT_STMNT"],[4,3,1,"","KEYWORD_LIST"],[4,3,1,"","KIND_SPEC"],[4,3,1,"","LINE_LABEL"],[4,3,1,"","LOGICAL"],[4,3,1,"","MOD"],[4,3,1,"","NON_DEF"],[4,3,1,"","NUMBER"],[4,3,1,"","OBJBREAK"],[4,3,1,"","PARAMETER_VAL"],[4,3,1,"","PP_ANY"],[4,3,1,"","PP_DEF"],[4,3,1,"","PP_DEF_TEST"],[4,3,1,"","PP_INCLUDE"],[4,3,1,"","PP_REGEX"],[4,3,1,"","PROCEDURE_STMNT"],[4,3,1,"","PROG"],[4,3,1,"","PRO_LINK"],[4,3,1,"","RESULT"],[4,3,1,"","SCOPE_DEF"],[4,3,1,"","SELECT"],[4,3,1,"","SELECT_DEFAULT"],[4,3,1,"","SELECT_TYPE"],[4,3,1,"","SQ_STRING"],[4,3,1,"","SUB"],[4,3,1,"","SUBMOD"],[4,3,1,"","SUB_MOD"],[4,3,1,"","SUB_PAREN"],[4,3,1,"","TATTR_LIST"],[4,3,1,"","THEN"],[4,3,1,"","TYPE_DEF"],[4,3,1,"","TYPE_STMNT"],[4,3,1,"","USE"],[4,3,1,"","VAR"],[4,3,1,"","VIS"],[4,3,1,"","WHERE"],[4,3,1,"","WORD"]],fortls:[[4,0,0,"-","constants"],[4,0,0,"-","ftypes"],[4,0,0,"-","helper_functions"],[4,0,0,"-","interface"],[4,0,0,"-","intrinsics"],[4,0,0,"-","json_templates"],[4,0,0,"-","jsonrpc"],[4,0,0,"-","langserver"],[4,0,0,"-","objects"],[4,0,0,"-","parse_fortran"],[4,0,0,"-","regex_patterns"],[4,0,0,"-","version"]]},objnames:{"0":["py","module","Python module"],"1":["py","data","Python data"],"2":["py","class","Python class"],"3":["py","attribute","Python attribute"],"4":["py","function","Python function"],"5":["py","method","Python method"],"6":["py","exception","Python exception"]},objtypes:{"0":"py:module","1":"py:data","2":"py:class","3":"py:attribute","4":"py:function","5":"py:method","6":"py:exception"},terms:{"0":[1,2,3,4,5],"1":[3,4,5,8,9],"10":[4,8],"101":5,"106":5,"109":5,"11":3,"112":5,"116":5,"119":5,"12":4,"13":5,"14":[4,5],"16":5,"169":5,"17":[4,5],"18":5,"184":5,"187":5,"188":5,"19":[4,9],"191":5,"2":[3,4],"200":5,"2022":9,"203":5,"206":5,"207":5,"21":1,"22":[4,5],"3":[1,4,5],"32601":4,"33":5,"34":5,"35":5,"36":5,"39":5,"3rd":2,"4":[4,5,8],"43":5,"46":5,"47":5,"48":5,"5":[2,4],"50":5,"51":5,"54":5,"55":5,"6":4,"60":5,"62":5,"63":5,"67":5,"7":[1,5],"76":5,"78":5,"8":5,"80":5,"9":[4,5],"99":5,"9_":4,"abstract":4,"boolean":4,"break":1,"case":[4,8],"class":[3,4,5],"const":4,"default":[2,4,5,8,9],"do":[0,2,4,8,9],"enum":[4,5],"export":4,"final":[2,4],"function":[1,2,3,4,5,8],"goto":[3,5],"import":[3,4,5],"int":4,"long":4,"new":4,"public":4,"return":[4,5],"short":4,"static":4,"true":[2,4],"try":[4,9],"var":4,"while":[1,5],A:[1,2,4,6,8],By:8,FOR:[4,8],For:[0,2,8,9],IF:4,IN:4,IS:4,If:[2,4,8,9],In:[1,4],Is:0,It:[1,4,5,9],NOT:9,Or:2,THEN:4,The:[2,4,5,8,9],Then:[1,2],There:1,To:[1,2,3,4,5,8,9],_:4,__literal_internal_dummy_var_:4,_hdf5:8,_tmp:8,about:[2,4],absolut:[4,5],across:[3,4,5],action:[2,4,5,8],ad:2,add:[2,4,5],add_child:4,add_doc:4,add_error:4,add_includ:4,add_int_memb:4,add_memb:4,add_priv:4,add_publ:4,add_rel:4,add_scop:4,add_subroutin:4,add_us:4,add_vari:4,addit:[2,4,5,8,9],adher:1,adjac:4,aesthet:5,after:4,against:4,alia:4,all:[1,2,4,5,8],allocat:4,allow:[1,2,5,8],allow_empti:4,along:4,alreadi:4,also:[1,2,4,8],altern:[8,9],alwai:[4,5,8],ambigu:4,an:[0,2,4,5],anaconda:[5,9],ancestor_nam:4,ani:[0,1,2,4,9],anim:5,anoth:4,anyth:1,append:[4,8],appli:4,apply_chang:4,ar:[0,1,2,4,5,8,9],arbitrari:[2,5],arg1:4,arg2:4,arg:[2,4],arg_list:4,arg_list_nam:4,arg_modifi:4,argpars:4,argument:[2,3,4,5],argumentpars:4,arrai:[4,5],ask:0,assign:4,associ:[4,5],associatemap:4,assum:8,ast:4,attach:[1,2],attempt:4,attribut:5,auto:[3,5],autocomplet:5,autocomplete_name_onli:8,autocomplete_no_prefix:[2,8],autocomplete_no_snippet:8,autogener:5,autom:5,automat:8,autoupd:[5,8],avail:[2,4,9],b:4,backward:4,bar:4,base:4,basic:5,becom:[1,6],been:[2,4,5],befor:[1,4],behaviour:8,being:[4,5,8],below:8,beta:8,between:4,binari:9,bind:4,bind_nam:4,bit:1,black:5,block:[3,4,5],block_id_stack:4,bodi:5,bool:4,both:4,bound:[3,5],bound_nam:4,bracket:4,branch:[1,5],broken:1,buffer:2,bug:[0,5],build:[1,4],c:[4,8,9],call:4,can:[0,1,2,4,5,8,9],cannot:[4,5,8],capabl:5,case_typ:4,caus:4,cd:4,certain:8,cfg:5,chang:[1,3,4],change_arg:4,change_json:4,change_str:4,channel:[4,5],char_po:4,charact:[4,8],check:[0,1,4,8],check_definit:4,check_fil:4,check_us:4,check_valid_par:4,checkout:1,child:4,choic:4,chosen:4,ci:5,class_var:4,classinfo:4,clearer:5,client:[2,4,5],climb_type_tre:4,clone:1,close:[3,4],close_fil:4,cmd:2,cmdline:2,code:[4,5,6,8,9],codeact:3,codecov:5,col:4,column:[4,5],com:[0,1,9],command:[2,4,5],commandline_arg:4,comment:[4,5,8],commit:[1,5],compact:1,compani:0,compil:4,complet:[2,4,5,6,8,9],complex:4,compliant:5,compon:4,conda:[5,9],condens:8,condit:4,conduct:[1,5],config:[2,5,8],configur:[2,5,6],conn:4,connect:4,consecut:4,consid:8,consider:1,consist:8,constant:[5,7],construct:4,contact:6,contain:[3,4,8],container_nam:4,content:7,contents_split:4,context:4,contigu:4,continu:[4,5],contribut:[5,6],control:2,convent:5,convert:4,copi:4,copy_from:4,copy_interfac:4,copy_sourc:4,correct:1,correspond:4,coverag:5,creat:[1,2,4],create_binding_vari:4,create_none_scop:4,curr_lin:4,curr_path:4,curr_scop:4,current:[4,8],cursor:[3,4],custom:2,d:4,data:4,dataclass:4,date:5,debug:4,debug_act:8,debug_char:8,debug_complet:8,debug_definit:8,debug_diagnost:8,debug_filepath:8,debug_full_result:8,debug_hov:8,debug_implement:8,debug_lin:8,debug_log:8,debug_pars:8,debug_refer:8,debug_renam:8,debug_rootpath:8,debug_signatur:8,debug_symbol:8,debug_workspace_symbol:8,debugg:[1,5],declar:4,def_char:4,def_fil:4,def_kind:4,def_lin:4,def_obj:4,defer:[3,4],deffin:8,defin:[2,3,4,5,8],definit:[2,3,4,5,8,9],depend:[5,9],deprec:[2,8],deque_find_and_pop:4,desc:4,desc_str:4,descript:[3,4],dest:4,detail:[2,8,9],detect:[3,4,8],detect_fixed_format:4,detect_format:4,determin:4,dev:[1,5],develop:[6,9],diagnost:[4,5,6],diagnostic_json:4,dict:4,dictionari:[4,8],did_clos:4,did_open:4,didchang:3,didclos:3,didopen:3,didsav:3,diff:5,differ:[4,8,9],dim_str:4,dimens:4,direct:5,directori:[1,4,8],disabl:[4,5,8],disable_autoupd:[5,8],disable_diagnost:8,discours:0,discuss:[0,8],disk:4,displai:[4,5,8],doc:[1,4],doc_str:4,docstr:4,document:[2,3,4,5,8],documenthighlight:[3,5],documentsymbol:[3,8],doe:[2,4],donat:[1,6],doubl:4,download:6,doxygen:[3,4],dparkin:2,dq_string:4,drop_arg:4,duck:0,dure:[1,2,4,5,8],e:[1,2,3,4,5,8],each:2,earli:5,easier:1,ech:4,edit:2,editor:[1,5,6,9],effect:8,either:[2,4],element:4,elif:4,elin:4,eln:4,els:4,emac:5,empti:4,enabl:[2,4,5,8],enable_code_act:8,enc_scop:4,enclos:4,encod:5,encount:4,end:[4,5],end_associ:4,end_block:4,end_do:4,end_enumd:4,end_fix:4,end_fun:4,end_if:4,end_int:4,end_mod:4,end_ppif:4,end_pro:4,end_prog:4,end_scop:4,end_scope_regex:4,end_select:4,end_smod:4,end_sub:4,end_typ:4,end_wher:4,end_word:4,endif:4,engin:2,ensur:1,entir:[1,4,8],enum_def:4,environ:5,err_msg:4,error:[3,4,5],etc:[1,3,4,8,9],eval:2,everi:1,everyth:4,exact_match:4,exampl:[1,4,9],exc_info:4,except:4,excl_path:5,exclud:[5,8],exclude_dir:8,execut:2,exist:4,exit:8,expand:4,expand_nam:4,expect:1,experiment:[3,8],express:4,extend:4,extens:[2,4,8,9],extern:[4,5,8],external_obj:4,extract:4,f03:[4,8],f05:[4,8],f08:[4,8],f18:[4,8],f2018:5,f2:2,f5:2,f77:[4,8],f90:[4,8],f95:[4,8],f:[4,5,8],factori:4,fals:[4,8],favourit:2,featur:[0,1,2,6,8,9],few:[1,2],field:4,field_nam:4,file:[1,2,4,5,9],file_ast:4,file_init:4,file_lin:4,file_obj:4,file_path:4,filepath:[4,8],filetyp:2,filter:8,filter_publ:4,find:[1,4],find_extern:4,find_external_attr:4,find_external_typ:4,find_in_scop:4,find_in_workspac:4,find_paren_match:4,find_word:4,find_word_in_code_lin:4,find_word_in_lin:4,first:[1,4],firstli:[1,2],fix:4,fixed_com:4,fixed_cont:4,fixed_doc:4,fixed_openmp:4,fixedform:2,flag:4,flake:5,fnmatch:[4,8],folder:[1,8],follow:[2,4,8,9],foo:4,foral:[4,5],ford:[3,4],forg:[5,9],fork:1,form:4,format:[1,4,5],fortl:[0,2,7,8,9],fortran90:8,fortran:[0,1,2,4,6,8,9],fortran_ast:4,fortran_fil:4,fortran_includ:4,fortran_liter:4,fortran_var:4,fortranast:4,fortranfil:4,fortranobj:4,fortranregularexpress:4,forward:4,found:[4,8,9],fpp:[4,8],fqsn:4,free:4,free_com:4,free_cont:4,free_doc:4,free_format_test:4,free_openmp:4,friendli:5,from:[1,2,3,4,5,8,9],ftype:7,full:[4,8,9],fun:4,fun_onli:4,fun_sig:4,funsig:4,further:5,fyp:8,g:[2,3,4,8],gcc:3,gd:2,gen_assign:4,gener:[3,4,8],generic_pro:4,genprocdefinfo:4,get:[2,3,4,6],get_act:4,get_all_refer:4,get_ancestor:4,get_children:4,get_code_lin:4,get_comment_regex:4,get_definit:4,get_desc:4,get_diagnost:4,get_docs_ful:4,get_docstr:4,get_document:4,get_enc_scope_nam:4,get_fortran_definit:4,get_hov:4,get_implicit:4,get_inner_scop:4,get_interfac:4,get_interface_arrai:4,get_intrinsic_keyword:4,get_keyword:4,get_lin:4,get_line_context:4,get_line_prefix:4,get_object:4,get_overridden:4,get_paren_level:4,get_paren_substr:4,get_placehold:4,get_procedure_modifi:4,get_scop:4,get_signatur:4,get_single_line_docstr:4,get_snippet:4,get_typ:4,get_type_obj:4,get_use_tre:4,get_var_stack:4,git:[1,2,9],github:[0,1,5,8,9],given:[3,4],glob:[4,5,8],glob_path:4,global:[2,5],gnikit:[0,1,2,5,8,9],go:[1,3,5],good:1,granular:5,group:4,guid:1,h:8,ha:[2,4,5],handl:[4,5],hansec:2,hash:4,have:[0,1,2,4,8,9],have_petsc:8,haven:1,help:[1,4,8],helper_funct:7,henc:2,here:[2,4],hidden:2,highlight:2,hold:4,holder:8,hook:[2,5],host:5,hover:[2,4,5,6,9],hover_arrai:4,hover_languag:[2,8],hover_req:4,hover_signatur:[2,8],how:[5,9],howev:[1,4,8,9],html:[4,8],http:[1,4,8],i:[2,4,5],id:[2,4,8],ident:8,ieee_arithmet:3,ieee_except:3,ieee_featur:3,ifdef:4,ifndef:4,ignor:4,ignorecas:4,implement:[1,3,5,8],implicit:[3,4],implicit_flag:4,implicitli:5,improv:5,impur:4,in_lin:4,incl_suffix:5,includ:[2,4,5,8],include_dir:[4,5],include_doc:4,includeinfo:4,incomplet:4,increment:8,incremental_sync:[2,8],index:[4,7,8],indic:4,individu:[1,5],infer:4,info:4,inform:[2,3,4,8,9],inherit_typ:4,inherit_vers:4,init:[2,4],init_var:4,initi:8,initialis:[4,5],inout:4,input:4,input_ext:4,inquiri:0,insid:[1,2],instal:[1,2,5,8,9],instantli:2,instead:[5,8],instruct:[2,5,9],int_onli:4,int_stmnt:4,integ:[4,8],integr:[5,6],intent:4,interact:1,interchang:8,interfac:[2,3,5,7,8,9],interface_str:4,interinfo:4,intermingl:5,interrupt:4,intersect:4,intrins:[5,7,8],invalid:[3,4],involv:1,io:[4,8],ios_c_bind:3,is_abstract:4,is_cal:4,is_external_int:4,is_mod_scop:4,is_opt:4,is_paramet:4,is_type_bind:4,is_type_region:4,iso_fortran_env:3,isort:5,issu:[0,1,5],item:4,its:[4,5,8],itself:4,json:[4,5,8],json_templ:7,jsonrpc2connect:4,jsonrpc2error:4,jsonrpc2protocolerror:4,jsonrpc:[1,7],just:2,k:2,kak:2,kak_sess:2,kakrc:2,kei:4,keybing:2,keyword:[4,5,8],keyword_info:4,keyword_list:4,kind:4,kind_spec:4,know:[0,4],known:9,known_typ:4,label:4,langserv:7,languag:[0,2,4,6,8,9],languageclient_servercommand:2,lcn:2,lead:[4,8],leader:2,leak:4,left:4,legaci:4,len:4,length:[5,8],let:[0,2],level:4,librari:4,like:[0,2,8,9],line:[4,5],line_label:4,line_no:4,line_numb:4,link:4,link_nam:4,link_obj:4,link_vers:4,list:[4,5],liter:[4,5],ln:4,load_from_disk:4,load_intrins:4,local:1,local_onli:4,locat:8,location_json:4,log:[4,5,8],logic:4,loglevel:5,logo:5,look:[1,4,8],loop:4,lower:8,lowercas:8,lowercase_intrins:8,lowercase_intris:2,lsp:2,lspconfig:2,lua:2,luckili:9,machin:1,mai:9,main:1,maintain:4,maintain_len:4,make:[0,1,2,6,9],manag:[2,4,9],mani:9,manifest:5,manner:8,map:[2,4],map_keyword:4,mark:[4,5],mark_contain:4,mask:3,master:5,mat:8,match:[4,5],max:5,max_comment_line_length:[4,8],max_line_length:[4,8],maximum:8,md5:4,md:5,meet:1,member:[4,8],menu:2,messag:[3,4,5,8,9],metavar:4,method:[1,4,5],metric:5,might:[4,9],min:9,miss:[0,3,5],mistak:1,mod:4,mod_flag:4,mod_mem:4,mod_nam:4,mod_onli:4,mod_word:4,mode:2,modern:[2,4,9],modifi:[2,4,5],modul:[5,7],modular:5,more:[2,5,6,8,9],msg:4,much:9,multilin:[4,5],multipl:[2,3,4,5],must:4,mutabl:5,my_project:8,myarrai:4,myvar:4,n:8,name:[2,3,4,5,9],name_replac:4,name_strip:4,narg:4,nativ:2,natur:4,navig:9,necessari:1,necessarili:4,need:[0,1,2,4,8,9],nest:3,new_scop:4,new_text:4,new_var:4,new_vi:4,newer:8,next:4,nmap:2,no_contain:4,no_link:4,node:4,non:[3,4],non_def:4,non_intrins:[4,5],none:4,nopass:4,noremap:2,normal:[1,4,8],note:[1,2,4],notif:[4,8],notify_init:[2,8],now:[1,2,5,8],nth:4,nthread:8,number:[0,4,8],numer:[4,9],obj_nam:4,obj_tre:4,objbreak:4,object:[3,5,7,8],occur:4,often:1,older:2,omp:4,omp_lib:[3,5],omp_lib_kind:[3,5],one:[8,9],ones:8,onli:[3,4],only_dir:4,only_list:4,open:[0,1,3,4],openacc:[3,5],openacc_kind:3,openmp:3,oper:[2,4],opposit:5,option:[2,4,5,6,9],option_str:4,order:5,org:4,other:[0,8],otherwis:4,our:[1,8],out:[0,4],outermost:4,output:4,over:[4,5,8],overal:5,overrid:[5,8],overriden:8,overwritten:8,packag:[2,5,7,9],pair:4,param:4,paramet:[4,5,8],parameter_v:4,parent:[3,4,5],parent_obj:4,parenthesi:[4,5],pars:[3,4,5],parse_contain:4,parse_do_fixed_format:4,parse_doc:4,parse_end_scope_word:4,parse_fortran:7,parse_implicit:4,parse_var_keyword:4,parser:[4,5,8],part:4,parti:2,partial:5,pass:[2,4],path:[1,2,4,5,8,9],path_from_uri:4,path_to_uri:4,pattern:[4,8],paus:1,paypal:1,peek:3,pend:4,ping:1,pip:[1,5,9],pipelin:4,place:[1,8],placement:3,pleas:[1,5],plug:2,plugin:2,pointer:[4,5],popular:2,posit:[3,4,8],possibl:[4,9],post:0,post_messag:4,potenti:4,pp_ani:4,pp_content:4,pp_def:[4,5],pp_def_test:4,pp_includ:4,pp_regex:4,pp_suffix:4,pr:1,pre:[1,5],pre_lin:4,preced:[4,8],precis:4,prefix:[4,8],prematur:5,prepar:5,preproc:4,preprocess:4,preprocess_fil:4,preprocessor:[3,4,5,9],present:4,preserve_keyword_ord:[5,8],prettier:5,previou:4,print:8,privat:[4,5],pro_lin:4,pro_link:4,procedur:[3,4,5,8],procedure_stmnt:4,process:4,produc:4,prog:4,program:4,project:[1,3,5,8,9],project_root_fil:2,propag:5,provid:[2,6,9],public_onli:4,pull:1,pure:4,push:1,py:[1,5],pypi:[5,8,9],pyproject:5,pytest:1,python:[1,4,5],pyupgrad:5,qs:4,queri:4,query_str:8,question:0,quot:4,rais:4,rang:4,range_json:4,rank:4,re:4,reach:0,reachabl:2,read:[4,5,9],read_associate_def:4,read_block_def:4,read_do_def:4,read_enum_def:4,read_fil:4,read_fun_def:4,read_generic_def:4,read_if_def:4,read_imp_stmt:4,read_inc_stmt:4,read_int_def:4,read_messag:4,read_mod_def:4,read_prog_def:4,read_rpc_messag:4,read_select_def:4,read_sub_def:4,read_submod_def:4,read_type_def:4,read_use_stmt:4,read_var_def:4,read_vis_stmnt:4,read_where_def:4,reader:4,readlin:4,readm:5,readwrit:4,real:4,reamd:5,reason:9,recommend:2,recurs:[4,8],redesign:5,refer:[6,8],reformat:5,regex:4,regex_pattern:7,regist:2,regular:4,reinstal:9,rel:4,releas:5,relev:4,rememb:2,remot:5,remov:8,renam:[2,4,5,6,8],rename_map:4,rename_str:8,report:[0,5],repositori:1,repres:[2,4],req_contain:4,req_named_end:4,request:[0,1,4,5,8],requir:[2,4],require_inherit:4,require_link:4,resolut:4,resolv:4,resolve_arg_link:4,resolve_glob:4,resolve_includ:4,resolve_inherit:4,resolve_link:4,respons:[1,4,8],rest:4,restructur:5,result:[4,5,8],result_modifi:4,result_nam:4,result_sig:4,result_typ:4,result_var:4,resultsig:4,review:1,rewrot:5,rid:4,root:[2,4,8],root_path:[4,8],rpc:4,run:[1,4],s:[2,4,5,8],same:[3,4,5,9],save:[3,4],scaled_vector:4,scan:8,sch:4,scope:[3,4,5],scope_def:4,scope_obj:4,search:[3,4],section:[2,4,5,8,9],see:[0,2,4,8,9],select:[1,4],select_default:4,select_info:4,select_typ:4,selectinfo:4,selector:2,self:4,semant:5,send:[1,8],send_diagnost:4,send_notif:4,send_request:4,send_request_batch:4,sent:4,separ:[2,4,5],separate_def_list:4,seper:4,sequenc:4,seri:8,serve_autocomplet:4,serve_codeact:4,serve_default:4,serve_definit:4,serve_document_symbol:4,serve_exit:4,serve_hov:4,serve_implement:4,serve_initi:4,serve_onchang:4,serve_onclos:4,serve_onopen:4,serve_onsav:4,serve_refer:4,serve_renam:4,serve_signatur:4,serve_workspace_symbol:4,server:[1,2,4,6,8,9],set:[2,4,5,8,9],set_cont:4,set_default_vi:4,set_dim:4,set_external_attr:4,set_implicit:4,set_inherit:4,set_keyword_ord:4,set_lowercase_intrins:4,set_par:4,set_parameter_v:4,set_vis:4,setact:4,settrac:5,setup:[2,5],setuptools_scm:5,sev:4,sever:4,sh:2,shield:5,should:[1,2,4,8],show:[3,5,8],sig:4,signatur:[4,5,8],signaturehelp:[3,8],silent:2,simpl:9,simplifi:5,simultan:9,sinc:[4,5,9],singl:[3,4],size:5,skip:[4,5],slightli:4,sline:4,sln:4,small:1,smodinfo:4,snippet:8,so:[1,2,8],sole:[2,4],some:[4,5],sort:[4,8],sort_keyword:[5,8],sourc:[1,2,5,9],source_dir:5,space:5,specif:8,specifi:[2,8],sphinx:5,split:4,sponsor:[1,6],spot:0,sq_string:4,src:8,src_file_ext:4,standard:1,standardis:5,start:[0,2,4,6,8],start_ppif:4,state:5,statement:[3,4,5],step:[1,9],sting:4,store:5,str:4,string:[4,5],strip:4,strip_com:4,strip_line_label:4,strip_str:4,strongli:2,structur:5,style:[3,4,8],sub1:4,sub:4,sub_info:4,sub_mod:4,sub_paren:4,subdirectori:8,subinfo:4,submit:[1,5],submod:4,submodul:[5,7],subroutin:[3,4,8],substitut:[5,8],succe:4,suffix:8,suggest:[2,3],suit:1,supercharg:6,support:[0,2,3,5,9],sure:[0,2],surround:5,symbol:6,symbol_json:4,symbol_skip_mem:[2,8],synchron:8,synchronis:3,syntax:[2,4],t:1,tab:[1,3],tag:[0,1,4],take:1,target:4,tattr_list:4,tcpreadwrit:4,technic:0,templat:3,termin:[1,4,5],test:[5,8],test_interfac:1,test_str:4,test_version_update_pypi:1,text:4,textdocu:[3,5,8],thank:1,thei:[4,5,8,9],theire:5,them:8,therefor:2,thi:[1,2,4,8,9],thread:[4,8],through:[1,2,4,5,8,9],thu:1,time:1,tip:1,tmat:8,todo:4,toggl:4,toml:[2,5],tool:[6,9],tradit:4,trail:4,treat:4,tree:4,trigger:5,troubl:9,tupl:[4,5],two:4,type:[3,4,5,8],type_def:4,type_mem:4,type_onli:4,type_stmnt:4,unclos:3,undef:4,under:8,unifi:5,unimpl:3,uninstal:9,unit:1,unittest:[1,5],unix:5,unknown:3,unlabel:4,unless:8,until:4,up:[4,5,9],updat:[1,4,5],update_fqsn:4,update_link:4,update_workspace_fil:4,upgrad:9,upon:[3,4,5],upper:8,uppercas:8,uri:4,uri_json:4,us:[1,2,3,4,5,6,9],usag:8,use_dict:4,use_info:5,use_lin:4,use_mod:4,use_signature_help:[2,8],useinfo:4,user:[3,4,5,9],usernam:1,usr:8,utf:5,v1:5,v3:3,v5:[3,5],v:[1,8],val:4,valid:4,valu:[4,5],var1:[4,8],var2:[4,8],var3:4,var_desc:4,var_kei:4,var_line_numb:4,var_nam:4,var_onli:4,var_stack:4,var_typ:4,variabl:[2,3,4,5,8],variable_hov:[5,8],varinfo:4,varis:1,verifi:1,version:[1,2,5,7,8,9],vi:4,via:[4,8,9],vim:[5,9],vimrc:2,vis_flag:4,visibl:[3,4],visinfo:4,vs17:2,vs:[5,9],vscode:8,w:4,wa:[2,5],wai:[0,1,4,5],walk:4,want:[1,2,4,8],warn:[4,5],we:[4,9],websit:5,were:5,what:8,when:[3,4,5,8],where:[1,4,5],whether:4,which:[1,4,5],wide:3,window:[2,5],winsetopt:2,without:[4,5],witout:5,word:4,work:[1,5,9],workflow:[5,9],workspac:[3,4,8],workspace_init:4,would:[0,5],write:[1,4],write_error:4,write_respons:4,write_rpc_notif:4,write_rpc_request:4,writer:4,xor_eq:4,ycm_language_serv:2,ycmfindsymbolindocu:2,ycmfindsymbolinworkspac:2,yfd:2,yfw:2,you:[0,1,2,4,8,9],your:[0,1,2,9],z0:4,z:4,z_:4},titles:["Contact Us","Contributing to fortls","Editor Integration","Features","fortls package","Unique fortls features (not in fortran-language-server)","fortls","Documentation","Configuration options","Get Started"],titleterms:{"2017":2,action:3,ad:5,all:3,argument:8,atom:2,autocomplet:8,avail:8,chang:5,code:[1,2,3],codeact:8,command:8,complet:3,configur:[8,9],constant:4,contact:0,content:4,contribut:1,debug:[1,8],depend:1,deprec:5,develop:1,diagnost:[3,8],document:7,download:[1,9],editor:2,emac:2,error:8,excl_path:8,excl_suffix:8,featur:[3,5],file:8,financi:1,find:3,fix:5,fortl:[1,4,5,6],fortran:5,ftype:4,get:[1,9],help:3,helper_funct:4,hover:[3,8],incl_suffix:8,include_dir:8,indic:7,integr:[2,9],interfac:4,intrins:[3,4],json_templ:4,jsonrpc:4,kakoun:2,langserv:4,languag:5,languagecli:2,limit:8,line:8,lsp:3,merg:1,modul:[3,4],name:8,neovim:2,object:4,onli:8,option:8,packag:4,pars:8,parse_fortran:4,pp_def:8,pp_suffix:8,preprocessor:8,refer:3,regex_pattern:4,renam:3,request:3,server:5,signatur:3,sourc:8,source_dir:8,start:[1,9],studio:2,sublim:2,submodul:4,support:1,swigl:8,symbol:[3,8],tabl:7,test:1,text:2,tmp:3,uniqu:5,us:[0,8],usag:9,version:4,vim:2,visual:2,youcompletem:2}})