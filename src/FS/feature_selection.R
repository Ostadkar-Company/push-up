get_optimal_features <- function(in_features){
    cur_trmse <- get_trmse(in_features)
    
    print('current model has these features:')
    print(in_features)
    print('-----')
    #print('assessing performance dropiing this feature:')
    print('with trmse: ')
    print('-----')
    print(cur_trmse)
    min_trmse = cur_trmse
    feature_to_drop = '-1'
    for(cur_feature in in_features){
        new_trmse = get_trmse(setdiff(in_features, cur_feature))
        print(paste0('trmse when dropping ', cur_feature, ' :'))
        print(new_trmse)
        if(min_trmse>=new_trmse){
            min_trmse = new_trmse
            feature_to_drop = cur_feature
        }
 
    }
  
    if(feature_to_drop!='-1'){
        print('-----')
        print(paste0("dropping ", cur_feature))
        print('-----')
        return(get_optimal_features(setdiff(in_features, cur_feature)))
    }
    if(feature_to_drop=='-1'){
        print('found optimal feature set')
        return(in_features)
    }
  
}
set.seed(1) 
get_optimal_features(setdiff(colnames(input_data), 'label_wn_quote'))

