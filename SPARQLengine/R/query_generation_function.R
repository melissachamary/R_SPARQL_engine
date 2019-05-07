#### TODO :
###########
#### 1. query parameter object ==> A checker utilité####
setClass("query_parameter", representation=
           representation(triple="data.frame", keep_variable="data.frame",description="data.frame",query_dependancy="data.frame"),
         prototype = prototype(
           triple = data.frame(subj=character(0),	pred=character(0),	obj=character(0),
                              scope=character(0),	mandatory=character(0),	refine_label=character(0)),
           keep_variable=data.frame(query_label=character(0),keep=character(0)),
           description=data.frame(query_label=character(0),	scope=character(0),	refine_label=character(0),	query_type=character(0)),
           query_dependancy=data.frame())

          )

###  2. getTemplateQueryParameter template (file & list) query parameter #####
#' @title getTemplateQueryParameter
#' @details Provide empty query_parameter object at R list (file=null) or xlsx format
#' @param file : path to file, if empty or invalid file path function will return a parameter list
#' @return query_parameter
#' @export
getTemplateQueryParameter<-function(file=NA){
  template<-list( query_list = data.frame(query_label=character(0), type=character(0)),
                  graph_list= data.frame(graph_label=character(0),uri=character(0)),
                  prefix_list= data.frame(prefix=character(0),uri=character(0)),

    triple = data.frame(subj=character(0),	pred=character(0),	obj=character(0),
                                           scope=character(0),	mandatory=character(0),	refine_label=character(0)),
                       keep_variable=data.frame(query_label=character(0),keep=character(0)),
                       description=data.frame(query_label=character(0),	scope=character(0),	refine_label=character(0),	query_type=character(0)),
                       #query_dependancy=data.frame()

                       graph_query= data.frame(query_label=character(0),graph_label=character(0)))
  if(is.na(file)||is.null(file)){

  }
  ##### CORRECT PATH
  else if(dir.exists(file)){
    ### PATH est un dosser
      completePath<-paste(file,sep="/","getTemplateQueryParameter.xlsx")
      warning(paste("[getTemplateQueryParameter] file parameter is directory. Template will be generated at path",
                    completePath))
      write.xlsx(template, file = completePath)

    }
  else if(!dir.exists(file)&& dir.exists(dirname(file))){
    #### Fichier à créer
    write.xlsx(template, file = file)

  }else if (file.exists(file)){
    ### Fichier déjà créer
    write.xlsx(template, file = file)

  }
  #### INCORRECT PATH
  else if(!(file.exists(file) && ! dir.exists(file))){
    ## PATH N'EST NI UN FICHIER NI UN DOSSIER
      warning("[getTemplateQueryParameter] file parameter invalid path")

  }
    return(template)
}


### 2.bis createQueryObject (from file function or ) #####
#' @title createQueryObjectListFromList
#' @details Function take a list of data.frame according the templateFormat and return
#' @param list : list of dataframe object
#' @return named list of SPARQL_query_builder objects (using query_Name data)
#' @export
createQueryObjectListFromList<-function(list=NA){
  query_type<-getMethodNames()
  query_type_allowed<-c(unlist(getMethodNames()),NA)
  data<-getTemplateQueryParameter()
  query_list<-list()
  check<-TRUE
  error_message<-"[createQueryObjectList] Fatal error"
  if(is.null(list) || typeof(list)!=typeof(list())){
    stop("[createQueryObjectList] list argument missing or invalid")
  }

  else{
    ##### TEST Structure (Table & colonnes) ==> utilise la fonction getTemplate()
    if(all(names(data)%in% names(list))){
      for(i in names(data)){
        ref_colnames<-colnames(data[[i]])
        colnames<-colnames(list[[i]])
        if(all(! ref_colnames%in%colnames)){
          if(check){
            check<-FALSE
            error_message<- paste(error_message, sep="\n", " Missing columns in ")

          }else{
            error_message<- paste(error_message, sep="\n", paste(" \n \t table",
                                                                 paste(i,sep=" column : ",paste(ref_colnames[wich(! ref_colnames%in%colnames)], collapse=""))))
          }

        }
      }
      if(!check){
        stop(error_message)
      }

    ##### Test contenu et création des objets query
    ##### 1. Query Graph & Prefix unicity of id & method normalization
      ## Query_label unicity
      if(length(unique(list$query_list$query_label))!=length(list$query_list$query_label)){
        check<-FALSE
        error_message<- paste(error_message, sep="\n", "query_list$query_label should be unique")
      }
      ### query_type check and normalize (used further to specific check)
      if(all(!normalize(list$query_list$type)%in%query_type_allowed)){
        error_message<- paste(error_message, sep="\n", "unknown type value in query_list sheet ",
                              paste(unique(list$query_list$type[!normalize(list$query_list$type)
                                                                %in% query_type_allowed]),collapse=", "))
      }
      else{
        list$query_list$type<- matchMethod(list$query_list$type)
      }

      ## prefix unicity
      if(length(unique(list$prefix_list$prefix))!=length(list$prefix_list$prefix)){
        check<-FALSE
        error_message<- paste(error_message, sep="\n", "prefix_list$prefix should be unique")
      }
      ## graph unicity
      if(length(unique(list$graph_list$graph_label))!=length(list$graph_list$graph_label)){
        check<-FALSE
        error_message<- paste(error_message, sep="\n", "graph_list$graph_label should be unique")
      }
      ### Check all graph in graph_query exist in graph list
      if(all(!unique(list$graph_query$graph_label)%in%list$graph_list$graph_label)){
        check<-FALSE
        error_message<- paste(error_message, sep="\n", paste("missing graph_query$graph_label in graph_list ",
                                                             paste(unique(list$graph_query$graph_label[which(!(list$graph_query$graph_label)
                                                                                                             %in%list$graph_list$graph_label)]),collapse=", ")))
      }
      if(!check){
        stop(error_message)
      }

      query_name<-unique(list$query_list$query_label)
      query_list<-vector(length(unique(list$query_list$query_label)),mode = "list")
      names(query_list)<-unique(query_name)

      ### Construction des query
      for(i in query_name){
        query<-NULL
        check<-TRUE
        ## Data extraction
        graph<-vector(nrow(unique(list$graph_query[which(list$graph_query$query_label==i),])),mode = "list")
        names(graph)<-unique(list$graph_query$graph_label[which(list$graph_query$query_label==i)])
        for(g in names(graph)){
          graph[[g]]<-list$graph_list$uri[which(list$graph_list$graph_label==g)]
        }

        ### Construction des query object par type de query
        if(list$query_list$type[which(list$query_list$query_label==i)]%in%query_type[["graph_query"]]){
          ### graph_query : 1 graphe unique
          ### Pas de préfixe
            if(length(graph)!=1){
              check<-FALSE
              error_message<- paste(error_message, sep="\n", paste(i,"multiple or non graph referenced by a SPARQL graph query"))

            }
              query <-new(Class = "SPARQL_query_builder", prefix=data.frame(),
                        graph=graph,method=list$query_list$type[which(list$query_list$query_label==i)])

          }

        else if(list$query_list$type[which(list$query_list$query_label==i)]%in%query_type[["data_mng"]]){
          ### data_mng : graph (0-1)
          ### Prefix & graph
          if(length(graph)>1){
            check<-FALSE
            error_message<- paste(error_message, sep="\n", paste(i,"multiple or non graph referenced by data insertion query"))
          }
            query <-new(Class = "SPARQL_query_builder", prefix=list$prefix_list,
                        graph=graph,method=list$query_list$type[which(list$query_list$query_label==i)])


        }

        else if(list$query_list$type[which(list$query_list$query_label==i)]%in%query_type[["data_query"]]){
          #### data_query : check data_linkage OK

          #### Prefix, Graph,
           # if(!all(any(list$keep_variable$query_label==i),
            #        any(list$description$query_label==i))){
          #### Autorise keep_variable = NA avec un warning
          keep<-list$keep_variable$keep[which(list$keep_variable$query_label==i)]
          ### Check la descriptin de la query
          if(! any(list$description$query_label==i)){
              check<-FALSE
              error_message<- paste(error_message, sep="\n", paste(i,"query isn't described in query description data.frame"))
          }else{
            #### test scope unique
            scope_i<-list$description$scope[which(list$description$query_label==i)]
            refine_i<-list$description$refine_label[which(list$description$query_label==i)]

            if(length(scope_i)!=1){
              error_message<- paste(error_message, sep="\n", paste(i,"query description set none or multiple scope wherease 1 unique is required"))
            }

            data.constraint<-list$triple[intersect(
              which(list$triple$scope %in% scope_i),
              which(normalize(list$triple$mandatory) == "TRUE")),c("subj","pred","obj"),with=FALSE]
            data.constraint<-unique(rbind(data.constraint,
                                          list$triple[
                                            intersect(which(list$triple$scope %in% scope_i),
                                                      which(list$triple$refine_label%in% refine_i))
                                            ,c("subj","pred","obj"),with=FALSE]
            ))
            print(head(data.constraint))
            query<-new(Class = "SPARQL_query_builder", prefix=list$prefix_list,
                               graph=graph,method=list$query_list$type[which(list$query_list$query_label==i)])

            query<-addConstraintFrame(query, data.constraint)
          }

        }


        if(check){
          query_list[[i]]<-query
        }else{
          warning(paste("[createQueryObjectList] query ",
                        paste(i,
                              paste("not added du to following violated constraint during object construction",
                                    error_message))))
        }
      }

      ### Vérification
      ####
    }else{
      stop(paste("[createQueryObjectList] missing mandatory data.frame in list :"), sep=" ",
           paste(names(data)[which(!names(data)%in% names(list))], collapse=', '))
    }

  }
  return(query_list)
}

#' @title createQueryObjectListFromFile
#' @details Function take a excel according the templateFormat and return query_parameter object
#' @param file : path to xlsx file
#' @return named list of SPARQL_query_builder objects (using query_Name data)
#' @export
createQueryObjectListFromFile<-function(file=NA){
  data<-getTemplateQueryParameter()
  if(is.na(file)||is.null(file) || !file.exists(file) || is.na(format_from_ext(file)) ){
    stop("[createQueryObjectList] file parameter invalid. Please check correctness of file argument, file path or file extention (xlsx,xls) ")
  }else{
    if (all(names(data)%in% excel_sheets(path = file))){
        for(i in  names(data)){
          data[[i]]<-data.table(data.frame(read_excel(path=file,sheet=i,na = ""), stringsAsFactors = FALSE))
        }
    }else{
      stop(paste("[createQueryObjectList] missing mandatory excel sheet in file :"), sep=" ",
           paste(names(data)[which(!names(data)%in% excel_sheets(path = file))], collapse=', '))
    }
  }
  query_list<-createQueryObjectListFromList(data)

  return(query_list)
}

###  3. generate_query_objectList#####
###
######
generate_query_list<-function(parameters){
  graph_list<-parameters$graph
  prefix_frame<-parameters$prefix
  constraintDataFrame<-as.data.frame(read_excel(path=parameters$query_parameter_file,sheet = "query-triple"))
  query_Description<-as.data.frame(read_excel(path=parameters$query_parameter_file,sheet = "query_description"))
  keepQueryFrame<-as.data.frame(read_excel(path=parameters$query_parameter_file,sheet = "query_keep_variable"))
  queryList<-vector(length(unique(query_Description$query_Name)),mode = "list")
  names(queryList)<-unique(query_Description$query_Name)

  for(i in names(queryList)){
    data.constraint<-constraintDataFrame[intersect(
      which(constraintDataFrame$scope == unique(query_Description$scope[which(query_Description$query_Name == i)])),
      which(constraintDataFrame$mandatory=="true")),c("subj","pred","obj")]
    data.constraint<-unique(rbind(data.constraint,
                                  constraintDataFrame[
                                    intersect(which(constraintDataFrame$scope == unique(query_Description$scope[which(query_Description$query_Name == i)])),
                                              which(constraintDataFrame$`filter-refine`%in% unique(query_Description$refine[which(query_Description$query_Name == i)]))
                                    ),c("subj","pred","obj")] ) )
    keep<-keepQueryFrame$keep[which(keepQueryFrame$queryName==i)]
    query<-new(Class = "SPARQL_query_builder", prefix=prefix_frame,
               graph=graph_list,method='SELECT')
    queryList[[i]]<-addConstraintFrame(query,data = data.constraint,keep = keep)

  }
  return(queryList)
}
