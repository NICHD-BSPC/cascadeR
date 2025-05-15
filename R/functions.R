#' Create access yaml
#'
#' This function creates an access yaml file.
#' This is primarily intended for the first run.
#'
#' @param user User name
#' @param user_group User group
#' @param data_area Path to data area containing RDS files
#'
#' @export
create_access_yaml <- function(user, user_group, data_area){
    ug <- setNames(as.list(user_group), user)
    da <- setNames(as.list(data_area), user_group)

    path <- get_access_path()

    write_yaml(list(user_group=ug, data_area=da),
               path)
}

#' Read access yaml with user groups and data areas
#'
#' This function reads the access yaml file and
#' returns user groups and data areas
#' as a list of data frames.
#'
#' @export
read_access_yaml <- function(){
    # figure out access file based on host
    f <- get_access_path()

    # check if yaml exists
    if(!file.exists(f)){
        stop('Access yaml not found. Have you run "create_access_yaml()" yet?')
    }

    al <- read_yaml(f)

    return(al)
}

#' Save access yaml to file
#'
#' This function saves access details (user groups
#' and data areas) to the designated access yaml file.
#'
#' @param lst list of data frames with user_groups and
#'  data_areas
#'
#' @export
save_access_yaml <- function(lst){
    # get access file
    f <- get_access_path()

    write_yaml(list(user_group=lst$user_group,
                    data_area=lst$data_area), f)
}

#' Get project name from path
#'
#' This function takes in a path to an RDS file and returns
#' a string to be used as project name
#'
#' So if the path is: /path/to/project/test/clustered.Rds
#' and depth=2 & end_offset=0
#' this returns: project/test
#'
#' @param x character path to RDS file
#' @param depth integer how many levels below path to look?
#' @param end_offset integer how far from the end of path to end?
#' @param staging_dir name of staging directory
#' @param fsep file separator to split path with
#'
#' @return project name
#'
#' @export
#'
get_project_name_from_path <- function(x,
                                       depth=2, end_offset=0,
                                       staging_dir='staged',
                                       fsep=.Platform$file.sep){
  d <- dirname(x)

  # split path by fsep
  tok <- unlist(strsplit(d, fsep))

  # if path contains staging_dir, increase depth
  if(any(tok == staging_dir)){
    depth <- depth + 1
  }

  # join upto 'level' elements from the end
  end_idx <- length(tok) - end_offset
  start_idx <- length(tok) - depth + 1

  p <- paste(tok[start_idx:end_idx], collapse=fsep)

  p
}

#' Get limits of numeric vector
#'
#' This function calculates the limits of a numeric vector
#' and returns them after rounding to specified number of
#' significant digits.
#'
#' @param gq numeric vector
#' @param sig_digits significant digits for rounding
#' @param na.rm remove NAs? Default: TRUE
#'
#' @return vector of length 2 with min, max
get_limits <- function(gq,
                       sig_digits=3,
                       na.rm=TRUE){
  fctr <- 10**sig_digits
  min_gq <- floor(min(gq, na.rm=na.rm)*(fctr))/(fctr)
  max_gq <- ceiling(max(gq, na.rm=na.rm)*(fctr))/(fctr)

  return(c(min_gq, max_gq))
}

#' Sanitize strings
#'
#' This function removes special characters from strings.
#' Meant to be used to sanitize column names of a data frame
#' before plotting.
#'
#' @param cnames column names to sanitize
#' @param bad_char regex with characters to be replaced with 'repl'
#' @param repl replacement character
#'
#' @return sanitized string vector
#'
sanitize_colnames <- function(cnames,
                              bad_char='(\\-|\\+|\\/|\\*|\\:|\\-|\\.)',
                              repl='_'){

  sanitized_names <- sapply(1:length(cnames),
                       function(x) gsub(bad_char, repl, cnames[x])
                     )

  return(sanitized_names)
}


#' Interactive umap plot
#'
#' @param df data.frame with plotting data
#' @param xcol x-axis coordinates
#' @param ycol y-axis coordinates
#' @param color column to color markers by
#' @param colors colors to use for markers
#' @param split metadata column to split by
#' @param label_cols Additional metadata columns to use for labeling pts
#' @param showlegend should legend be shown?
#' @param showticklabels should tick labels be drawn?
#' @param type type of plotly trace (default: 'scatter')
#' @param highlight_click should points be highlighted on clicking
#' @param marker_size size of markers
#' @param margin margin of plot (should be between 0 & 1)
#' @param alpha marker opacity
#' @param free_axes should subplots have free axes?
#' @param width width of plot in pixels. If NULL (default), plot is auto-sized.
#' @param height height of plot in pixels. if NULL (default), plot is auto-sized.
#' @param source name of source to return data from
#'
#' @return plotly handle
#'
umap_ly <- function(df, xcol, ycol,
                    color,
                    colors,
                    split=NULL,
                    label_cols=NULL,
                    showlegend=TRUE,
                    showticklabels=TRUE,
                    type='scatter',
                    highlight_click=TRUE,
                    marker_size=2,
                    margin=0.03,
                    alpha=0.3,
                    free_axes=FALSE,
                    width=NULL,
                    height=NULL,
                    source='A'){

  if(inherits(df, 'data.table')) df <- as.data.frame(df)

  # sanitize plotting column names
  cols_to_sanitize <- c(xcol, ycol, color)
  new_names <- sanitize_colnames(cols_to_sanitize)

  cidx <- which(colnames(df) %in% cols_to_sanitize)
  colnames(df)[cidx] <- new_names
  xcol <- new_names[1]
  ycol <- new_names[2]
  color <- new_names[3]

  xlims <- range(df[, xcol])
  ylims <- range(df[, ycol])

  xdelta <- diff(xlims)*0.05
  ydelta <- diff(ylims)*0.05

  xrange <- round(xlims + c(-xdelta, xdelta))
  yrange <- round(ylims + c(-ydelta, ydelta))

  # build axis titles
  axis_titles <- list(
                    list(x=0.5,
                         y=-2*margin,
                         xref='paper',
                         yref='paper',
                         xanchor='center',
                         yanchor='bottom',
                         showarrow=FALSE,
                         font=list(size=17),
                         text=xcol),
                    list(x=-1.2*margin,
                         y=0.5,
                         xref='paper',
                         yref='paper',
                         xanchor='left',
                         yanchor='center',
                         textangle=-90,
                         showarrow=FALSE,
                         font=list(size=17),
                         text=ycol)
                 )

  # build hover text
  if(is.null(label_cols)) label_cols <- color
  ll <- lapply(label_cols, function(x){
          if(is.numeric(df[, x])) paste0('<b>', x, ': ', round(df[, x], 3), '<b>')
          else paste0('<b>', x, ': ', as.character(df[, x]), '<b>')
        })
  hover_text <- do.call(paste, args=c(ll, sep='\n'))

  # drop any missing levels from color column (if factor)
  curr_lvls <- unique(as.character(df[, color]))
  if(is.factor(df[, color])){
    factor_lvls <- levels(df[, color])

    if(any(!factor_lvls %in% curr_lvls)){
      curr_lvls <- intersect(factor_lvls, curr_lvls) # this maintains original order
      df[, color] <- factor(df[, color], levels=curr_lvls)
    }
  }

  # convert to order of colors in legend
  curr_lvls <- intersect(names(colors), curr_lvls)

  if(is.null(split)){
    if(highlight_click){
      df <- df %>%
        highlight_key(
          as.formula(paste('~', color)))
    }
    p <- plot_ly(df,
              source=source,
             x=as.formula(paste('~', xcol)),
             y=as.formula(paste('~', ycol)),
             type=type,
             text=hover_text,
             mode='markers',
             hoverinfo='text',
             color= as.formula(paste('~', color)),
             colors=colors,
             alpha=alpha,
             width=width,
             height=height,
             showlegend=showlegend,
             marker=list(size=marker_size))

    if(highlight_click){
      p <- p %>%
        highlight(on=NULL)
        #highlight(on='plotly_click',
        #          off='plotly_deselect',
        #          color='gray40')
        # TODO: do something with the selection?
    }

    p <- p %>%
      layout(annotations=axis_titles,
        xaxis=list(range=xrange, title='',
                   tickfont=list(size=13),
                   showline=TRUE,
                   showticklabels=showticklabels,
                   linewidth=1,
                   showgrid=FALSE,
                   zeroline=FALSE),
        yaxis=list(range=yrange, title='',
                   tickfont=list(size=13),
                   showline=TRUE,
                   showticklabels=showticklabels,
                   linewidth=1,
                   showgrid=FALSE,
                   zeroline=FALSE),
        dragmode='lasso',
        legend=list(itemsizing='constant'))
  } else {
    if(sum(duplicated(colnames(df))) == 0){
      ddf <- df %>%
        dplyr::group_by(df[, split])

      lvls <- attr(ddf, 'groups')[[1]]
    } else {
      ddf <- df

      idx <- grep(split, colnames(ddf))
      if(is.factor(ddf[, idx[1]])){
        lvls <- levels(ddf[, idx[1]])

        # only keep lvls that are present
        lvls <- lvls[lvls %in% unique(ddf[, idx[1]])]
      } else {
        lvls <- unique(ddf[, idx[1]])
      }
    }

    if(length(lvls) == 2){
      nrows <- 1
      ncols <- 2
    } else {
      nrows <- ceiling(sqrt(length(lvls)))
      ncols <- round(sqrt(length(lvls)))
    }


    p <- lapply(lvls, function(x){
           idx <- ddf[, split] == as.character(x)

           if(!free_axes){
             xrange_split <- xrange
             yrange_split <- yrange
           } else {
             xlims_split <- range(ddf[idx, xcol])
             ylims_split <- range(ddf[idx, ycol])

             xrange_split <- round(xlims_split + c(-xdelta, xdelta))
             yrange_split <- round(ylims_split + c(-ydelta, ydelta))

           }

           tmp <- ddf[idx,]
           hover_text_tmp <- NULL
           if(showlegend){
             # add points from colors missing from legend outside axis limits
             # NOTE: this is to make sure that the legend for the
             #       first subplot shows all colors
             if(!all(curr_lvls %in% unique(tmp[[ color ]]))){
               missing_cols <- setdiff(curr_lvls, unique(tmp[[ color ]]))

               # build fake point df with as many rows as missing colors
               missing_df <- tmp[rep(1, length(missing_cols)), ]
               missing_df[, color] <- missing_cols
               missing_df[, xcol] <- rep(xrange_split[2] + 1, nrow(missing_df))
               missing_df[, ycol] <- rep(yrange_split[2] + 1, nrow(missing_df))

               tmp <- rbind(tmp, missing_df)
               tmp[[color]] <- factor(tmp[[color]], levels=curr_lvls)

               hover_text_tmp <- rep('', nrow(missing_df))

             }
           }

           p <- plot_ly(tmp,
                       source=source,
                       x=as.formula(paste('~', xcol)),
                       y=as.formula(paste('~', ycol)),
                       type=type,
                       text=c(hover_text[idx], hover_text_tmp),
                       mode='markers',
                       hoverinfo='text',
                       color= as.formula(paste('~', color)),
                       colors=colors,
                       alpha=alpha,
                       width=width,
                       height=height,
                       legendgroup=as.formula(paste('~', color)),
                       showlegend=showlegend,
                       marker=list(size=marker_size)) %>%
               layout(
                  xaxis=list(title='', range=xrange_split,
                             showgrid=FALSE, zeroline=FALSE,
                             showline=TRUE, linewidth=1,
                             showticklabels=showticklabels,
                             tickfont=list(size=12)),
                  yaxis=list(title='', range=yrange_split,
                             showgrid=FALSE, zeroline=FALSE,
                             showline=TRUE, linewidth=1,
                             showticklabels=showticklabels,
                             tickfont=list(size=12)),
                  legend=list(itemsizing='constant')
               )

             # reset global showlegend after first plot
             if(showlegend) showlegend <<- FALSE

             p
          })

    shareX <- FALSE
    shareY <- FALSE

    # NOTE: this is the slow step
    p <- subplot(p, nrows=nrows,
                 shareX=shareX, shareY=shareY,
                 margin=margin)

    # get plot margins
    xdelta <- 1/ncols
    ydelta <- 1/nrows
    xloc <- rep(xdelta*(0:(ncols-1)), nrows) + 0.6*xdelta
    yloc <- rep(ydelta*((nrows):1), each=ncols) #+ 0.05*ydelta

    # arrange plot titles
    subplt_titles <- lapply(1:length(lvls),
                       function(x){
                         list(x=xloc[x], y=yloc[x],
                              xref='paper', yref='paper',
                              xanchor='center', yanchor='top',
                              showarrow=FALSE,
                              text=paste('<b>',lvls[x],'<b>'),
                              font=list(size=12))
                       })

    # generate layout
    p <- p %>% layout(annotations=c(axis_titles, subplt_titles),
                      dragmode='lasso')

  }

  p
}

#' Binned coexpression plot for 2 features
#'
#' This is a wrapper around umap_ly
#' that uses four colors for four categories
#' instead of a blend matrix.
#'
#' @param df data.frame with plotting data
#' @param xcol x-axis coordinates
#' @param ycol y-axis coordinates
#' @param blend_cols columns to blend
#' @param colors 3 color vector (geneA, geneB, merge)
#' @param split metadata column to split plot by
#' @param col_threshold_1 blending threshold for gene 1
#' @param col_threshold_2 blending threshold for gene 2
#' @param n number of bins for color range
#' @param bin_mode how to bin? can be 'range' (gene expression binned into
#'        n bins) or 'quantile' (cells binned into n bins by exp)
#' @param neutral_color double negative color
#' @param showlegend should legend be shown?
#' @param showticklabels should tick labels be drawn?
#' @param type type of plotly trace (default: 'scattergl')
#' @param marker_size size of markers
#' @param margin margin of plot (should be between 0 & 1)
#' @param alpha marker opacity
#' @param free_axes should subplots have free axes?
#' @param width width of plot in pixels. If NULL (default), plot is auto-sized.
#' @param height height of plot in pixels. if NULL (default), plot is auto-sized.
#'
#' @return plotly handle
#'
feature_blend <- function(df, xcol, ycol, blend_cols,
                          colors,
                          split=NULL,
                          col_threshold_1=0.5,
                          col_threshold_2=0.5,
                          n=100,
                          bin_mode='range',
                          neutral_color='lightgray',
                          showlegend=TRUE,
                          showticklabels=TRUE,
                          type='scattergl',
                          marker_size=2,
                          margin=0.03,
                          alpha=0.3,
                          free_axes=FALSE,
                          width=NULL,
                          height=NULL){

  if(inherits(df, 'data.table')) df <- as.data.frame(df)

  # get joint column
  if(length(blend_cols) != 2){
    stop('Number of features to blend must be 2')
  }

  bin_exp <- get_binned_exp(df[, blend_cols], bins=n, mode=bin_mode)

  # build color column
  thres_bin_1 <- col_threshold_1*n
  thres_bin_2 <- col_threshold_2*n

  g1_idx <- as.numeric(bin_exp[,1]) < thres_bin_1
  g2_idx <- as.numeric(bin_exp[,2]) < thres_bin_2

  col_idx <- list('neither'=which(g1_idx & g2_idx),
                  'g1 only'=which(!g1_idx & g2_idx),
                  'g2 only'=which(g1_idx & !g2_idx),
                  'both'=which(!g1_idx & !g2_idx))
  names(col_idx)[2] <- paste(blend_cols[1], 'only')
  names(col_idx)[3] <- paste(blend_cols[2], 'only')

  color <- rep('', nrow(df))
  for(i in 1:length(col_idx)){
    color[col_idx[[i]]] <- rep(names(col_idx)[i], length(col_idx[[i]]))
  }
  df[[ 'color' ]] <- factor(color, levels=names(col_idx))
  cols.use <- c(neutral_color, colors)
  names(cols.use) <- names(col_idx)

  p <- umap_ly(df, xcol=xcol, ycol=ycol,
               color='color',
               colors=cols.use,
               split=split,
               label_cols=blend_cols,
               showlegend=showlegend,
               showticklabels=showticklabels,
               type=type,
               highlight_click=FALSE,
               marker_size=marker_size,
               margin=margin,
               alpha=alpha,
               free_axes=free_axes,
               width=width,
               height=height)

  p

}

#' Get counts table based on coexpression
#'
#' Get data frame with counts of cells
#' binned into four categories based on expression of
#' two genes
#'
#' - both genes low
#' - gene A low, gene B high
#' - gene A high, gene B low
#' - gene A high, gene B high
#'
#' @param df data.frame with gene expression data
#' @param genes genes to calculate coexpression for
#' @param n number of bins
#' @param threshold1 percentile to define expression for gene 1
#' @param threshold2 percentile to define expression for gene 2
#' @param bin_mode how to bin? can be 'range' (gene expression binned into
#'        n bins) or 'quantile' (cells binned into n bins by exp)
#'
#' @return data.frame with co-expression counts
#'
get_coexp_tbl <- function(df, genes, n = 100,
                          threshold1=0.5, threshold2=0.5,
                          bin_mode='range'){

  if(inherits(df, 'data.table')) df <- as.data.frame(df)

  if(length(genes) > 2){
    stop('Need exactly 2 genes to get coexpression')
  }

  bin_exp <- get_binned_exp(df[, genes], bins=n, mode=bin_mode)

  thres_bin1 <- threshold1*n
  thres_pct1 <- threshold1*100

  thres_bin2 <- threshold2*n
  thres_pct2 <- threshold2*100

  g1_idx <- as.numeric(bin_exp[,1]) < thres_bin1
  g2_idx <- as.numeric(bin_exp[,2]) < thres_bin2
  counts <- c(sum(g1_idx & g2_idx),
              sum(!g1_idx & g2_idx),
              sum(g1_idx & !g2_idx),
              sum(!g1_idx & !g2_idx))
  pct <- counts*100/nrow(df)

  desc <- c(paste0(genes[1], ' < ', thres_pct1, '%; ',
                   genes[2], ' < ', thres_pct2, '%'),
            paste0(genes[1], ' > ', thres_pct1, '%; ',
                   genes[2], ' < ', thres_pct2, '%'),
            paste0(genes[1], ' < ', thres_pct1, '%; ',
                   genes[2], ' > ', thres_pct2, '%'),
            paste0(genes[1], ' > ', thres_pct1, '%; ',
                   genes[2], ' > ', thres_pct2, '%'))
  labels <- c('neither',
              paste(genes[1], 'only'),
              paste(genes[2], 'only'),
              'both')
  pct_df <- data.frame(
              row.names=desc,
              labels=labels,
              counts=counts,
              pct=pct)
  colnames(pct_df) <- c('labels', '# cells', '%')
  return(pct_df)
}

#' Bin gene expression in columns of a data frame
#'
#' @param df data.frame with expression data
#' @param bins number of bins
#' @param mode how to calculate bins? options are 'range' or 'quantile'
#'
#' @return data.frame
#'
get_binned_exp <- function(df, bins=100, mode='range'){
  bin_fun <- function(x){
    if(mode == 'range'){
      return(round(x = (bins - 1) * (x - min(x))/(max(x) - min(x))))
    } else if(mode == 'quantile'){
      qq <- quantile(x, probs=seq(0, 1, by=1/bins))
      binned <- cut(x, breaks=qq, include.lowest=TRUE, right=FALSE)
      return(as.factor(as.numeric(binned)))
    } else {
      stop('"mode" can only be "range" or "quantile"')
    }
  }

  df <- as.data.frame(x = apply(X = df, MARGIN = 2, FUN = bin_fun))
  df
}

#' Interactive blend map plot
#'
#' Seurat:::BlendMap function converted to plotly.
#'
#' NOTE: this is not used
#'
#' @param color.matrix matrix of colors
#' @param dimnames axis labels
#'
#' @return plotly handle
#'
BlendMap2 <- function(color.matrix, dimnames=NULL){
  color.heat <- matrix(data = 1:prod(dim(x = color.matrix)) -
      1, nrow = nrow(x = color.matrix), ncol = ncol(x = color.matrix),
      dimnames = list(1:nrow(x = color.matrix), 1:ncol(x = color.matrix)))

  ll <- list()
  txt <- list()
  mat_dims <- c(nrow(color.matrix), ncol(color.matrix))
  for(i in 1:mat_dims[1]){
    ll[[i]] <- list()
    txt[[i]] <- list()
    for(j in 1:mat_dims[2]){
      xind <- mat_dims[1] - i + 1
      yind <- j
      ll[[i]][[j]] <- as.vector(col2rgb(color.matrix[xind, yind]))
      if(is.null(dimnames)){
        txt[[i]][[j]] <- as.character(color.heat[xind, yind])
      } else {
        txt[[i]][[j]] <- paste0(dimnames[1], ' ~ ', xind*mat_dims[1], '%\n',
                                dimnames[2], ' ~ ', yind*mat_dims[2], '%')
      }
    }
  }

  p <- plot_ly(z=ll,
               text=txt,
               hoverinfo='text',
               type='image') %>%
       layout(
        xaxis=list(title=dimnames[1],
                   yref='y',
                   y=-1,
                   tickfont=list(size=10),
                   showline=FALSE,
                   showticklabels=FALSE,
                   #linewidth=1,
                   showgrid=FALSE,
                   zeroline=FALSE),
        yaxis=list(title=dimnames[2],
                   xref='x',
                   x=-1,
                   tickfont=list(size=10),
                   showline=FALSE,
                   showticklabels=FALSE,
                   #linewidth=1,
                   showgrid=FALSE,
                   zeroline=FALSE)
       )
  p
}

#' Generalized version of Seurat:::BlendExpression
#'
#' Seurat:::BlendExpression for arbitrary number of bins
#' n = 1, means 10 bins
#' n = 2, means 100 bins
#' ...
#'
#' NOTE: not used
#'
#' @param data 2-column matrix with gene expression to be blended
#' @param n integer, defines number of bins = 10^n (default=1)
#'
#' @return data.frame with blended expression
#'
BlendExpression2 <- function (data, n=1){
    if (ncol(x = data) != 2) {
        stop("'BlendExpression' only blends two features")
    }
    features <- colnames(x = data)
    data <- as.data.frame(x = apply(X = data, MARGIN = 2, FUN = function(x) {
        return(round(x = (10^n - 1) * (x - min(x))/(max(x) - min(x))))
    }))
    data[, 3] <- data[, 1] + data[, 2] * (10^n)
    colnames(x = data) <- c(features, paste(features, collapse = "_"))
    for (i in 1:ncol(x = data)) {
        data[, i] <- factor(x = data[, i])
    }
    return(data)
}

#' Function to generate coexpression plot legend
#'
#' This interactive legend is shown in the sidebar to help
#' visually select coexpression blend threshold.
#'
#' @param colors 3-color vector (geneA, geneB, both)
#' @param dimnames names to show on x & y axis
#' @param xline x-axis threshold
#' @param yline y-axis threshold
#' @param n number of bins
#' @param neutral_color color to use for negative cells
#' @param margin plot margin
#'
#' @return plotly handle
get_coexp_legend <- function(colors,
                             dimnames,
                             xline,
                             yline,
                             n=100,
                             neutral_color='lightgray',
                             margin=0.03){

  vline <- function(x, color='black', width=0.75){
    list(
      type='line',
      y0=-0.1,
      y1=1.1,
      yref='paper',
      x0=x,
      x1=x,
      line=list(color=color, dash='dot', width=width)
    )
  }

  hline <- function(y, color='black', width=0.75){
    list(
      type='line',
      y0=y,
      y1=y,
      xref='paper',
      x0=-0.1,
      x1=1.1,
      line=list(color=color, dash='dot', width=width)
    )
  }

  tt <- list(x=xline*n,
             y=0.7,
             xref='x',
             yref='paper',
             showarrow=TRUE,
             xanchor='center',
             yanchor='center',
             text='<b>threshold</b>',
             font=list(color='black'))

  # build filled rectangles
  neutral <- list(
               type='rect',
               fillcolor=neutral_color,
               line=list(color=neutral_color),
               opacity=0.5,
               x0=0, x1=xline*n,
               y0=0, y1=yline*n,
               xref='x',
               yref='y')

  both <- list(
            type='rect',
            fillcolor=colors[3],
            line=list(color=colors[3]),
            opacity=0.5,
            x0=xline*n, x1=n,
            y0=yline*n, y1=n,
            xref='x',
            yref='y')

  genex <- list(
             type='rect',
             fillcolor=colors[1],
             line=list(color=colors[1]),
             opacity=0.5,
             x0=xline*n, x1=n,
             y0=0, y1=yline*n,
             xref='x',
             yref='y')

  geney <- list(
             type='rect',
             fillcolor=colors[2],
             line=list(color=colors[2]),
             x0=0, x1=xline*n,
             opacity=0.5,
             y0=yline*n, y1=n,
             xref='x',
             yref='y')



  p1 <- plot_ly(type='scatter', mode = 'markers') %>%
        layout(annotations=tt,
               margin=list(l=0.1, r=0, b=0.1, t=1),
               xaxis=list(title=dimnames[1], showline=FALSE,
                          showgrid=FALSE, zeroline=FALSE),
               yaxis=list(title=dimnames[2], showline=FALSE,
                          showgrid=FALSE, zeroline=FALSE),
               shapes=c(list(neutral), list(both),
                        list(genex), list(geney),
                        list(vline(xline*n)),
                        list(hline(yline*n)))
        )

  p1
}

#' Interactive feature plot
#'
#' @param df data.frame with expresion data & plot coordinates
#' @param xcol x-axis coordinates
#' @param ycol y-axis coordinates
#' @param color color column. should be numeric
#' @param colors plotly color scale. should be continuous
#' @param crange range of gene expression to show on plot
#' @param split column to split plots by
#' @param label_cols Additional metadata columns to use for labeling pts
#' @param row_view how should plots be laid out? can be 'single' or 'auto'
#' @param showscale show the color scale?
#' @param reversescale should we reverse the color scale?
#' @param showticklabels should tick labels be drawn?
#' @param marker_size marker size
#' @param margin plot margin
#' @param alpha marker opacity
#' @param free_axes should subplots have free axes?
#' @param reorder should be sort cells in ascending order of expression?
#' @param width width of plot in pixels
#' @param height height of plot in pixels
#'
#' @return plotly handle
feature_ly <- function(df, xcol, ycol,
                       color, colors,
                       crange=NULL,
                       split=NULL,
                       label_cols=NULL,
                       row_view='auto', # can be 'single' or 'auto'
                       showscale=TRUE,
                       reversescale=FALSE,
                       showticklabels=TRUE,
                       marker_size=2,
                       margin=0.03,
                       alpha=0.3,
                       free_axes=FALSE,
                       reorder=TRUE, # sort df in ascending order of expr
                       width=NULL,
                       height=NULL){

  if(inherits(df, 'data.table')) df <- as.data.frame(df)

  xlims <- range(df[, xcol])
  ylims <- range(df[, ycol])

  xdelta <- diff(xlims)*0.05
  ydelta <- diff(ylims)*0.05

  xrange <- round(xlims + c(-xdelta, xdelta))
  yrange <- round(ylims + c(-ydelta, ydelta))

  if(is.null(crange)) crange <- range(df[, color])

  # build hover text
  if(is.null(label_cols)) label_cols <- color
  ll <- lapply(label_cols, function(x){
          if(is.numeric(df[, x])) paste0('<b>', x, ': ', round(df[, x], 3), '<b>')
          else paste0('<b>', x, ': ', as.character(df[, x]), '<b>')
        })
  hover_text <- do.call(paste, args=c(ll, sep='\n'))

  # if gene name starts with a number, add character
  col_idx <- which(colnames(df) == color)
  if(regexpr('^\\d+', color) > 0){
    repl <- paste0('X', color)
    colnames(df)[col_idx] <- repl
    color <- repl
  }

  # sanitize plotting column names
  cols_to_sanitize <- c(xcol, ycol, color)
  new_names <- sanitize_colnames(cols_to_sanitize)

  cidx <- which(colnames(df) %in% cols_to_sanitize)
  colnames(df)[cidx] <- new_names
  xcol <- new_names[1]
  ycol <- new_names[2]
  color <- new_names[3]

  if(reorder) df <- df[order(df[, color]),]
  if(is.null(split)){
    p <- plot_ly(df,
                 x=as.formula(paste('~', xcol)),
                 y=as.formula(paste('~', ycol)),
                 type='scattergl',
                 text=hover_text,
                 mode='markers',
                 hoverinfo='text',
                 showlegend=FALSE,
                 alpha=alpha,
                 width=width,
                 height=height,
                 marker=list(size=marker_size,
                             cmin=crange[1],
                             cmax=crange[2],
                             colorscale=colors,
                             reversescale=reversescale,
                             color=as.formula(paste('~', color)),
                             showscale=showscale)) %>%
         layout(
            xaxis=list(range=xrange,
                       title='',
                       showline=TRUE, linewidth=1,
                       showticklabels=showticklabels,
                       showgrid=FALSE, zeroline=FALSE),
            yaxis=list(range=yrange,
                       title='',
                       showline=TRUE, linewidth=1,
                       showticklabels=showticklabels,
                       showgrid=FALSE, zeroline=FALSE),
            dragmode='lasso')


    axis_titles <- list(
                      # color variable (gene name)
                      list(x=-margin,
                           y=0.5,
                           xref='paper',
                           yref='paper',
                           xanchor='left',
                           yanchor='center',
                           textangle=-90,
                           showarrow=FALSE,
                           font=list(size=17),
                           text=paste('<b>', color,'<b>'))
                   )

    p <- p %>% layout(annotations=axis_titles)
  } else {

    ddf <- df %>%
      dplyr::group_by(df[, split])

    lvls <- attr(ddf, 'groups')[[1]]

    # NOTE: this is the original version which lays out
    #       plots in a square grid. Switching to single row
    #       view to allow multi-gene views.
    if(row_view == 'auto'){
      if(length(lvls) == 2){
        nrows <- 1
        ncols <- 2
      } else {
        nrows <- ceiling(sqrt(length(lvls)))
        ncols <- round(sqrt(length(lvls)))
      }
    } else if(row_view == 'single'){
      nrows <- 1
      ncols <- length(lvls)
    }

    if(split != color){
      legend_fun <- function(){
        if(showscale){
          showscale <<- FALSE
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    } else {
      legend_fun <- function(){
        return(TRUE)
      }
    }

    p <- lapply(lvls, function(x){
           idx <- ddf[, split] == as.character(x)
           if(showscale)
             showscale <- ifelse(x == lvls[1], legend_fun(), FALSE)

           if(!free_axes){
             xrange_split <- xrange
             yrange_split <- yrange
           } else {
             xlims_split <- range(ddf[idx, xcol])
             ylims_split <- range(ddf[idx, ycol])

             xrange_split <- round(xlims_split + c(-xdelta, xdelta))
             yrange_split <- round(ylims_split + c(-ydelta, ydelta))

           }

           p <- plot_ly(ddf[idx, ],
                       x=as.formula(paste('~', xcol)),
                       y=as.formula(paste('~', ycol)),
                       type='scattergl',
                       text=hover_text[idx],
                       mode='markers',
                       hoverinfo='text',
                       showlegend=FALSE,
                       alpha=alpha,
                       width=width,
                       height=height,
                       marker=list(size=marker_size,
                                   cmin=crange[1],
                                   cmax=crange[2],
                                   colorscale=colors,
                                   reversescale=reversescale,
                                   color= as.formula(paste('~', color)),
                                   coloraxis=color,
                                   showscale=showscale)) %>%
                layout(
                   xaxis=list(title='', range=xrange_split,
                              showline=TRUE, linewidth=1,
                              showticklabels=showticklabels,
                              showgrid=FALSE, zeroline=FALSE),
                   yaxis=list(title='', range=yrange_split,
                              showline=TRUE, linewidth=1,
                              showticklabels=showticklabels,
                              showgrid=FALSE, zeroline=FALSE)#,
                )
            }) # lapply

    # NOTE: this is the slow step for large data & many subplots
    p <- subplot(p,
                 nrows=nrows,
                 margin=margin)

    xdelta <- 1/ncols
    ydelta <- 1/nrows
    xloc <- rep(xdelta*(0:(ncols-1)), nrows) + 0.5*xdelta
    yloc <- rep(ydelta*((nrows):1), each=ncols)

    subplt_titles <- lapply(1:length(lvls),
                            function(x){
                              list(x=xloc[x],
                                   y=yloc[x],
                                   xref='paper',
                                   yref='paper',
                                   xanchor='center',
                                   yanchor='top',
                                   showarrow=FALSE,
                                   text=paste('<b>',lvls[x],'<b>'),
                                   font=list(size=12))
                            }
                          )

    axis_titles <- list(
                      # color variable (gene name)
                      list(x=-margin,
                           y=0.5,
                           xref='paper',
                           yref='paper',
                           xanchor='left',
                           yanchor='center',
                           textangle=-90,
                           showarrow=FALSE,
                           font=list(size=17),
                           text=paste('<b>', color,'<b>'))
                   )

    p <- p %>% layout(annotations=c(axis_titles, subplt_titles))

  }
  p
}

#' Violin plot
#'
#' Lightweight reimplementation of violin plot with additional splitting features.
#' This works on just a data frame and does not need to be
#' passed the entire object.
#'
#' @param df data.frame with expression data
#' @param xcol x-axis grouping column
#' @param ycol values to show on y-axis
#' @param color column to split violins by
#' @param colors vector of colors to use (not used)
#' @param draw_points show points for each cell? Default: FALSE
#' @param scales should y-axis be 'fixed' (default) or 'free'
#' @param text_scale scaling factor for text labels. If < 1, text size is reduced and vice-versa
#'
#' @return ggplot2 handle
violin2 <- function(df, xcol, ycol,
                    color=NULL, colors=NULL,
                    draw_points=FALSE,
                    scales='fixed',
                    text_scale=1){

  for(col in c(xcol, color)){
    # drop missing levels
    if(!is.null(col)){
      if(is.factor(df[, col])){
        curr_lvls <- levels(df[, col])
        df[, col] <- factor(df[, col],
                            levels=intersect(curr_lvls,
                                             unique(df[, col])))
      }
    }
  }

  if(is.null(color)) color <- xcol

  p <- ggplot(df, aes_string(x=xcol, y=ycol, fill=color)) +
    geom_violin(scale='width', adjust=1, trim=TRUE, size=0.1)

  if(!is.null(colors)) p <- p + scale_fill_manual(values=colors)

  if(draw_points){
    if(color != xcol){
      p <- p + geom_jitter(size=0.01, color='black',
                 position=position_jitterdodge(jitter.width=0.4,
                                               dodge.width=0.9))
    } else {
      p <- p + geom_jitter(size=0.01, color='black',
                 position='jitter')
    }
  }

  p <- p +
    facet_wrap(~variable, ncol=1, scales=scales) +
    ylab('Expression') +
    theme_bw() +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title.x=element_text(size=15*text_scale),
          axis.title.y=element_text(size=15*text_scale),
          axis.text.x=element_text(size=12*text_scale),
          axis.text.y=element_text(size=12*text_scale),
          legend.title=element_text(size=13*text_scale),
          legend.text=element_text(size=10*text_scale),
          strip.text.x=element_text(size=15*text_scale, face='bold'))

  p

}

#' Interactive violin plot
#'
#' Violin plot implemented in plotly.
#' This works fine, but is a bit slower/takes the same
#' time as the ggplot version. But the main issue is that
#' combining multiple plots using subplot() results in very
#' skinny violins.
#' - This might be a good option for single gene plots.
#'
#' NOTE: not used
#'
#' @param df data.frame with expression data
#' @param xcol x-axis grouping column
#' @param ycol values to show on y-axis
#' @param color column to split violins by
#' @param colors vector of colors to use (not used)
#' @param showlegend boolean, should legend be shown? Default: FALSE
#'
#' @return plotly handle
violin_ly <- function(df, xcol, ycol,
                      color,
                      colors,
                      showlegend=FALSE){

  for(col in c(xcol, color)){
    # drop missing levels
    if(is.factor(df[, col])){
      curr_lvls <- levels(df[, col])
      df[, col] <- factor(df[, col],
                          levels=intersect(curr_lvls,
                                           unique(df[, col])))
    }
  }

  if(is.null(color)) color <- xcol

  p <- df %>%
    plot_ly(x=as.formula(paste('~', xcol)),
            y=as.formula(paste('~', ycol)),
            color=as.formula(paste('~', color)),
            type='violin',
            spanmode='hard',
            points='all',
            pointpos=0,
            legendgroup=as.formula(paste('~', color)),
            scalegroup=as.formula(paste('~', color)),
            showlegend=showlegend,
            scalemode='width',
            spanmode='hard',
            offsetgroup=as.formula(paste('~', color)),
            alignmentgroup=as.formula(paste('~', xcol)),
            colors=colors,
            jitter=0.4,
            alpha=1,
            marker=list(color='black', size=1),
            line=list(size=0.1))

  p <- p %>%
    layout(
      yaxis = list(
        zeroline = F
      ),
      violinmode = 'group',
      violingroupgap = 0
    )

  p
}

#' Dotplot
#'
#' Lightweight dotplot reimplementation to run only on data frame with
#' gene expression and metadata.
#'
#' @param df data.frame with gene expression data
#' @param xcol x-axis grouping variable
#' @param ycol genes to show (on y-axis). can be multiple
#' @param split faceting variable
#' @param scale should data be scaled? Default: TRUE
#' @param dot.scale dot size scaling factor (default: 10)
#' @param col.min if data is scaled, this is the lower limit of values (default: -2.5)
#' @param col.max if data is scaled, this is the upper limit of values (default: 2.5)
#'
#' @return ggplot2 handle
dotplot <- function(df,
                    xcol, # grouping variable
                    ycol, # gene names (can be multiple)
                    split=NULL, # faceting variable
                    scale=TRUE,
                    dot.scale=10,
                    col.min=-2.5,
                    col.max=2.5){
  for(col in c(xcol)){
    # drop missing levels
    if(!is.null(col)){
      if(is.factor(df[, col])){
        curr_lvls <- levels(df[, col])
        df[, col] <- factor(df[, col],
                            levels=intersect(curr_lvls,
                                             unique(df[, col])))
      }
    }
  }

  # get levels of grouping variable
  if(!is.factor(df[, xcol]))
    df[, xcol] <- factor(df[, xcol])
  lvls <- levels(df[, xcol])

  # build a grouping column
  # - if not splitting, this is just the grouping variable
  # - else, paste group & split column
  if(is.null(split)){
    df[, 'grouping_column'] <- df[, xcol]
    lvls_df <- lvls
  } else {
    # get levels of split column
    if(is.factor(df[, split])) split_lvls <- levels(df[, split])
    else split_lvls <- unique(df[, split])

    # these are the current levels present
    df[, 'grouping_column'] <- paste0(df[, xcol], '|', df[, split])
    lvls_df <- unique(df[, 'grouping_column'])
  }

  df2 <- lapply(ycol, function(x){
           tmp <- df[, x]
           ll <- lapply(lvls_df, function(y){
                   idx <- df[, 'grouping_column'] == y

                   c(mean(tmp[idx]),
                     sum(tmp[idx] > 0)*100/length(tmp[idx]))
                 })
           ldf <- do.call('rbind', ll)

           if(scale){
             avg.scale <- scale(ldf[, 1])
             avg.scale[avg.scale < col.min] <- col.min
             avg.scale[avg.scale > col.max] <- col.max
           } else {
             avg.scale <- log1p(ldf[, 1]) # is this needed?
           }

           # split lvls_df into columns
           if(!is.null(split)){
             lvls_df2 <- as.data.frame(
                          do.call('rbind',
                            strsplit(lvls_df, '\\|')
                          )
                        )
             colnames(lvls_df2) <- c(xcol, split)
           } else {
             lvls_df2 <- data.frame(lvls_df)
             colnames(lvls_df2) <- xcol
           }
           df <- data.frame(gene=x,
                            lvls_df2,
                            avg.exp=avg.scale,
                            pct.exp=ldf[, 2])
         })
  df3 <- do.call('rbind', df2)

  df3[, xcol] <- factor(df3[, xcol], levels=lvls)
  df3[, 'gene'] <- factor(df3[, 'gene'], levels=ycol)

  if(!is.null(split)) df3[, split] <- factor(df3[, split], levels=split_lvls)

  p <- ggplot(df3, aes_string(x=xcol, y='gene')) +
    geom_point(aes_string(size='pct.exp', color='avg.exp')) +
    scale_radius(range=c(0, dot.scale), limits=c(NA, NA)) +
    theme_bw() +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank())

  if(!is.null(split))
    p <- p + facet_wrap(as.formula(paste0('~', split)), ncol=1) +
         theme(strip.text.x=element_text(size=12, face='bold'))

  p <- p +
    guides(size=guide_legend('Percent Expressed', order=2),
           color=guide_colorbar('Average Expression', order=1))
  p
}

#' Get plotly label trace
#'
#' Plotly trace to add labels to existing plot.
#'
#' @param plot_data plotly plot data
#' @param labeled_pts points to label
#' @param split column by which plot is split
#'
#' @return plotly handle
get_label_trace <- function(plot_data, labeled_pts,
                            split=FALSE){

  # subset plot data df to labeled barcodes
  bc <- unique(unlist(labeled_pts))
  if(inherits(plot_data$data, 'data.table')){
    idx <- plot_data$data$barcode %in% bc
    ldf <- plot_data$data[idx,]
  } else {
    ldf <- plot_data$data[bc, ]
  }
  ldf <- as.data.frame(ldf)

  color <- plot_data$color
  if(is.numeric(ldf[, color]))
    ht <- paste0('<b>', color, ': ',
                 round(ldf[, color], 3), '<b>')
  else
    ht <- paste0('<b>', color, ': ',
                 as.character(ldf[, color]), '<b>')
  hover_text <- paste(ht, sep='\n')

  if(!split){
    new_trace <- list(x=ldf[, plot_data$xcol],
                      y=ldf[, plot_data$ycol],
                      text=hover_text,
                      type='scatter',
                      mode='markers',
                      inherit=FALSE,
                      opacity=plot_data$alpha + 0.5,
                      name='selected',
                      marker=list(color='black',
                                  size=plot_data$marker_size),
                      showlegend=FALSE)
  } else {
    new_trace <- list()
    if(is.factor(ldf[, plot_data$split])){
      lvls <- levels(ldf[, plot_data$split])
    } else {
      lvls <- unique(ldf[, plot_data$split])
    }

    # NOTE: same logic as umap
    if(length(lvls) == 2){
      nrows <- 1
      ncols <- 2
    } else {
      nrows <- ceiling(sqrt(length(lvls)))
      ncols <- round(sqrt(length(lvls)))
    }

    # plot indices
    pind <- list()
    for(i in 1:nrows){
      for(j in 1:ncols){
        pind[[ length(pind) + 1 ]] <- c(i, j)
      }
    }
    names(pind) <- lvls

    new_trace <- list()
    for(i in 1:length(lvls)){
      grp <- lvls[i]
      ind <- ldf[, plot_data$split] == grp
      if(sum(ind) > 0){
        lldf <- ldf[ind, ]
        pos <- pind[[ grp ]]

        trace <- list(x=lldf[, plot_data$xcol],
                      y=lldf[, plot_data$ycol],
                      text=hover_text,
                      type='scatter',
                      mode='markers',
                      inherit=FALSE,
                      opacity=plot_data$alpha + 0.3,
                      name='selected',
                      marker=list(color='gray',
                                  size=plot_data$marker_size),
                      showlegend=FALSE)

        new_trace[[ grp ]] <- list(trace=trace,
                                   pos=pind[[grp]])
      }

    }
  }

  return(
    new_trace
  )
}

#' Get highly variable genes
#'
#' This function calculates most variable genes from either Seurat or anndata objects.
#'
#' @param obj object to get variable genes from
#' @param obj_type type of object being used. can be 'seurat' or 'anndata'
#' @param assay_name name of assay for variable genes. Ignored if obj_type == 'anndata'.
#'
#' @return vector of gene names
#'
get_var_genes <- function(obj, obj_type, assay_name){

  if(obj_type == 'seurat'){
    if(assay_name == 'SCT'){
      # NOTE: only using first model
      df <- obj@assays$SCT@SCTModel.list[[1]]
      var_genes <- rownames(df@feature.attributes)
    } else {
      vf <- VariableFeatures(obj, assay=assay_name)
      if(length(vf) == 0){
        obj <- FindVariableFeatures(obj, assay=assay_name)
      }
      var_genes <- VariableFeatures(obj, assay=assay_name)
    }
  } else if(obj_type == 'anndata'){
    # first check if obj$var has 'highly_variable' column
    if('highly_variable' %in% colnames(obj$var)){
      idx <- which(obj$var[[ 'highly_variable' ]])
      var_genes <- rownames(obj$var)[idx]
    } else {
      # NOTE: this is an expensive operation for large data
      vf <- FindVariableFeatures(Matrix::t(obj$X))
      sort_col <- grep('variance\\.standardized', colnames(vf))

      # sort by 'vst.variance.standardized'
      vf <- vf[order(vf[, sort_col]), ]
      var_genes <- rownames(vf)
    }
  }

  return( var_genes )
}

#' Build cluster tree for anndata objects
#'
#' This is a stripped down version of Seurat::BuildClusterTree for
#' anndata objects. It takes the same arguments as the Seurat function.
#'
#' @param object anndata object
#' @param features genes to use for building cluster tree
#' @param dims dimensions to use. This is only used if reduction is pca.
#' @param reduction reduction to use for cluster tree
#' @param clust_column column to use for grouping cells
#'
#' @return phylogenetic tree
BuildClusterTree2 <- function(
  object,
  features = NULL,
  dims = NULL,
  reduction = "X_pca",
  clust_column = 'leiden'
){
  if (!is.null(x = dims)) {

    if(!reduction %in% names(object$obsm)){
      stop(paste0('Reduction: "', reduction, '" not found in object! ',
                  'Possible values are: ',
                  paste(names(object$obsm), sep=', ')))
    }
    embeddings <- object$obsm[[ reduction ]][, dims]
    idents <- object$obs[[ clust_column ]]
    all_cells <- rownames(object)

    rownames(embeddings) <- all_cells

    data.dims <- lapply(
      X = levels(object$obs[[ clust_column ]]),
      FUN = function(x) {
        cells <- all_cells[idents == x]
        if (length(x = cells) == 1) {
          cells <- c(cells, cells)
        }
        temp <- colMeans(x = embeddings[cells, ])
      }
    )
    data.dims <- do.call(what = 'cbind', args = data.dims)
    colnames(x = data.dims) <- idents
    data.dist <- dist(x = t(x = data.dims))
  } else {
    features <- intersect(x = features, y = colnames(object))
    data.avg <- PseudoBulkExpression2(
      object = object,
      features = features,
      group.by=clust_column
    )
    data.dist <- dist(x = data.avg[,features])
  }
  data.tree <- ape::as.phylo(x = hclust(d = data.dist))

  return(data.tree)
}

#' Get pseudobulk expression from anndata object
#'
#' This is a stripped-down lightweight version of Seurat's internal function
#' adapted for an anndata object that has genes in columns not rows.
#'
#' @param object anndata object
#' @param pb.method pseudo-bulk method. Can be 'average' (default) or 'aggregate'.
#' @param features features to analyze. Default is all features in the object
#' @param group.by metadata column to group cells by
#'
#' @return matrix with pseudo-bulk counts
PseudoBulkExpression2 <- function(
  object,
  pb.method = 'average',
  features = NULL,
  group.by = 'ident'
){

  # get metadata columns for grouping variable(s)
  if(length(group.by) == 1){
    data <- data.frame(row.names=rownames(object),
                       object$obs[, group.by ])
    colnames(data) <- group.by
  } else {
    data <- object$obs[, rev(group.by)]
  }
  data <- data[which(rowSums(x = is.na(x = data)) == 0), , drop = F]
  if (nrow(x = data) < nrow(x = object)) {
    message("Removing cells with NA for 1 or more grouping variables")
    object <- object[rownames(data),]
  }
  for (i in 1:ncol(x = data)) {
    data[, i] <- as.factor(x = data[, i])
  }
  num.levels <- sapply(
    X = 1:ncol(x = data),
    FUN = function(i) {
      length(x = levels(x = data[, i]))
    }
  )
  if (any(num.levels == 1)) {
    message(
      paste0("The following grouping variables have 1 value and will be ignored: ",
             paste0(colnames(x = data)[which(num.levels <= 1)],
                    collapse = ", ")
      )
    )
    group.by <- colnames(x = data)[which(num.levels > 1)]
    data <- data[, which(num.levels > 1), drop = F]
  }
  if (ncol(x = data) == 0) {
    message("All grouping variables have 1 value only. Computing across all cells.")
    category.matrix <- matrix(
      data = 1,
      nrow = nrow(x = object),
      dimnames = list(rownames(object), 'all')
    )
    if (pb.method == 'average') {
      category.matrix <- category.matrix / sum(category.matrix)
    }
  } else {
    category.matrix <- Matrix::sparse.model.matrix(object = as.formula(
      object = paste0(
        '~0+',
        paste0(
          "data[,",
          1:length(x = group.by),
          "]",
          collapse = ":"
        )
      )
    ))
    colsums <- Matrix::colSums(x = category.matrix)
    category.matrix <- category.matrix[, colsums > 0]
    colsums <- colsums[colsums > 0]
    if (pb.method == 'average') {
      category.matrix <- sweep(
        category.matrix,
        MARGIN = 2,
        STATS = colsums,
        FUN = "/")
    }
    colnames(x = category.matrix) <- sapply(
      X = colnames(x = category.matrix),
      FUN = function(name) {
        name <- gsub(pattern = "data\\[, [1-9]*\\]", replacement = "", x = name)
        return(paste0(rev(x = unlist(x = strsplit(x = name, split = ":"))), collapse = "_"))
      })
  }

  # NOTE: using the 'X' slot here.
  # TODO: extend to allow use of 'layers'
  data.use <- object$X

  features.to.avg <- features %||% colnames(x = data.use)

  bad.features <- setdiff(x = features.to.avg, y = colnames(x = data.use))
  if (length(x = bad.features) > 0) {
    warning(
      "The following ", length(x = bad.features),
      " features were not found in the data",
      paste(bad.features, collapse = ", "),
      call. = FALSE, immediate. = TRUE)
  }
  features.assay <- intersect(x = features.to.avg, y = colnames(x = data.use))
  if (length(x = features.assay) > 0) {
    data.use <- data.use[,features.assay]
  } else {
    warning("None of the features specified were found in the assay.",
            call. = FALSE, immediate. = TRUE)
  }

  data.use <- expm1(x = data.use)
  if (any(data.use == Inf)) {
    warning("Exponentiation yielded infinite values. `data` may not be log-normed.")
  }

  data.return <- as.matrix(x = (Matrix::t(category.matrix) %*% data.use))

  return(data.return)
}

#' Add new cascade analysis
#'
#' @param obj_path path to Seurat/anndata object
#' @param data_dir output data directory. For convenience use the same data directory for all cascade projects.
#' @param project project name. Creates a subfolder with this name inside data_dir if it doesn't exist.
#' @param analysis analysis label. Creates a subfolder with this name inside data_dir/project
#' @param cluster_markers (optional) path to tab-delimited file containing cluster markers. Output from Seurat's FindAllMarkers
#'        and scanpy's rank_genes_groups are supported.
#' @param de_markers (optional) path to tab-delimited file containing differentially expressed markers from Seurat's FindMarkers or scanpy's
#'        rank_genes_groups.
#' @param conserved_markers (optional) path to tab-delimited file containing conserved markers from Seurat's FindConservedMarkers function.
#' @param overwrite boolean, if TRUE, existing analysis folder will be overwritten (default=FALSE)
#' @param execute boolean, set this to TRUE to actually run the commands (default=FALSE)
#'
#' @export
#'
add_cascade_analysis <- function(
                          obj_path,
                          data_dir,
                          project,
                          analysis,
                          cluster_markers=NULL,
                          de_markers=NULL,
                          conserved_markers=NULL,
                          overwrite=FALSE,
                          execute=FALSE){

  # check to see if obj is rds/h5ad file
  obj_file <- basename(obj_path)
  if(!grepl('\\.(rds|h5ad)$', tolower(obj_file))){
    stop(
      'Object "obj_file" does not appear to be RDS/h5ad file'
    )
  }

  # check to see if data_dir exists
  if(!dir.exists(data_dir)){
    stop(
      paste0('Data directory:"', data_dir, '" does not exist!')
    )
  } else {
    message(
      paste0('\n- Data directory:"', data_dir, '" already exists')
    )
  }

  # check to see if project directory exists, if not create it
  proj_path <- file.path(data_dir, project)
  if(!dir.exists(proj_path)){
    message(
      paste0('\n- Project directory:"', proj_path, '" does not exist. Creating it')
    )

    cmd <- paste0("mkdir -p ", proj_path)
    message(paste0('\n', cmd, '\n'))
    if(execute) system(cmd)
  } else {
    message(
      paste0('\n- Project directory:"', proj_path, '" already exists')
    )
  }

  # check analysis path
  analysis_path <- file.path(proj_path, analysis)
  if(dir.exists(analysis_path)){
    if(!overwrite){
      stop(
        paste0('\nAnalysis directory "', analysis_path, '" already exists! To overwrite, rerun with "overwrite=TRUE"')
      )
    } else {
      message(
        paste0('\n- Analysis directory "', analysis_path, '" exists, but "overwrite=TRUE". Removing it')
      )

      cmd <- paste0("rm -r ", analysis_path, '/*')
      message(paste0('\n', cmd, '\n'))
      if(execute){
        system(cmd)
      }
    }
  } else {
    message(
      paste0('- Analysis directory "', analysis_path, '" does not exist, creating it')
    )

    cmd <- paste0("mkdir -p ", analysis_path)
    message(paste0('\n', cmd, '\n'))
    if(execute) system(cmd)
  }

  # set up project using symlinks
  #
  # cd analysis_path
  # ln -s obj_path .
  # ln -s cluster_markers allmarkers.tsv
  # ln -s de_markers demarkers.tsv
  # ln -s conserved_markers consmarkers.tsv

  # save current directory
  if(execute) cwd <- getwd()

  # build command
  cmd <- c(paste0('cd ', analysis_path),
           paste0('ln -s ', obj_path, ' .'))

  if(!missing(cluster_markers)){
    if(!file.exists(cluster_markers)){
      stop(
        paste0('Cluster marker file: "', cluster_markers, '" does not exist!')
      )
    }
    cmd <- c(cmd, paste0('ln -s ', cluster_markers, ' allmarkers.tsv'))
  }

  if(!missing(de_markers)){
    if(!file.exists(de_markers)){
      stop(
        paste0('DE marker file: "', de_markers, '" does not exist!')
      )
    }
    cmd <- c(cmd, paste0('ln -s ', de_markers, ' demarkers.tsv'))
  }

  if(!missing(conserved_markers)){
    if(!file.exists(conserved_markers)){
      stop(
        paste0('Conserved marker file: "', conserved_markers, '" does not exist!')
      )
    }
    cmd <- c(cmd, paste0('ln -s ', conserved_markers, ' consmarkers.tsv'))
  }

  # print command or execute
  message('Setting up project:\n')
  message(paste(cmd, collapse='\n'))
  if(execute){
    system(paste(cmd, collapse='\n'))
    setwd(cwd)
  }

  if(!execute){
    message('\nIf everything looks good, rerun with "execute=TRUE" to create new analysis')
  } else {
    message(paste0('\nAll done! Remember to add "', data_dir, '" to Cascade data areas to view new analysis'))
  }
}


#' Function to return coexpression plot colormaps
#'
#' @param x character, name of color map. Can be 'red-blue' or 'red-green'
#'
#' @return 4 color vector: (neutral, gene A, gene B, merge)
get_coexplt_colors <- function(x){
  if(x == 'red-blue'){
    # purple, red, blue
    colors <- c('#d3d3d3', '#ff0000', '#0000ff', '#ff00ff')
  } else if(x == 'red-green'){
    # orange, red, darkgreen
    colors <- c('#d3d3d3', '#ff0000', '#006400', '#ff6400')
  }

  return(colors)
}

