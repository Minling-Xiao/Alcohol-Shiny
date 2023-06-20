
outlet_scenario_fun = function(join, outlet_dist, auckland=TRUE, area=NULL){
  if (!auckland){
    ind_list = vector("list",length(area))
    for (i in 1:length(area)){
      data = filter(join, Areas==area[i])
      mat = gDistance(as(data,"Spatial"),byid = T)
      diag(mat) = NA
      colnames(mat)=1:ncol(mat)
      rownames(mat)=1:nrow(mat)
      check = apply(mat,2,function(x)all(x>=outlet_dist,na.rm = T))
      ind = numeric(0)
      select_point = sample(1:ncol(mat),1,prob=rep(1/ncol(mat),ncol(mat)))
      id = colnames(mat)[select_point]
      ind = append(ind, id)
      remove = na.omit(which(mat[,select_point]<outlet_dist))
      if (length(remove)>0) mat = mat[-remove,-remove]
      check = apply(mat,2,function(x)all(x>=outlet_dist,na.rm = T))
      if (all(check)) output = colnames(mat)
      else{
        row_num = which.min(mat[,id])
        id = rownames(mat)[row_num]
        ind = append(ind, id)
        remove = na.omit(which(mat[,id]<outlet_dist))
        if (length(remove)>0) mat = mat[-remove,-remove]
        check = apply(mat,2,function(x)all(x>=outlet_dist,na.rm = T))
        while(!all(check)){
          row_num = which.min(rowMeans(mat[!rownames(mat)%in%ind,ind]))
          id = names(row_num)
          ind = append(ind, id)
          remove = na.omit(which(mat[,id]<outlet_dist))
          if (length(remove)>0)mat = mat[-remove,-remove]
          check = apply(mat,2,function(x)all(x>=outlet_dist,na.rm = T))
        }
        output = colnames(mat)
      }
      ind_list[[i]] = data[as.numeric(output),]
    }
    outlet_scenario = rbind(do.call(rbind,ind_list),filter(join, !Areas%in%area))
  } else{
    mat = gDistance(as(join,"Spatial"),byid = T)
    diag(mat) = NA
    colnames(mat)=1:ncol(mat)
    rownames(mat)=1:nrow(mat)
    check = apply(mat,2,function(x)all(x>=outlet_dist,na.rm = T))
    ind = numeric(0)
    select_point = sample(1:ncol(mat),1,prob=rep(1/ncol(mat),ncol(mat)))
    id = colnames(mat)[select_point]
    ind = append(ind, id)
    remove = na.omit(which(mat[,select_point]<outlet_dist))
    if (length(remove)>0) mat = mat[-remove,-remove]
    check = apply(mat,2,function(x)all(x>=outlet_dist,na.rm = T))
    if (all(check)) output = colnames(mat)
    else{
      row_num = which.min(mat[,id])
      id = rownames(mat)[row_num]
      ind = append(ind, id)
      remove = na.omit(which(mat[,id]<outlet_dist))
      if (length(remove)>0) mat = mat[-remove,-remove]
      check = apply(mat,2,function(x)all(x>=outlet_dist,na.rm = T))
      while(!all(check)){
        row_num = which.min(rowMeans(mat[!rownames(mat)%in%ind,ind]))
        id = names(row_num)
        ind = append(ind, id)
        remove = na.omit(which(mat[,id]<outlet_dist))
        if (length(remove)>0)mat = mat[-remove,-remove]
        check = apply(mat,2,function(x)all(x>=outlet_dist,na.rm = T))
      }
      output = colnames(mat)
    }
    outlet_scenario = join[as.numeric(output),]
  }
  
  return(outlet_scenario)
  
}
