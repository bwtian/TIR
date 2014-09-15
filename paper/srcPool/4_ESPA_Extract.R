library(teamlucc)

library(foreach)
library(iterators)
library(doParallel)

registerDoParallel(4)

start_dates <- as.Date(c('1988/1/1',
                         '1993/1/1',
                         '1998/1/1',
                         '2003/1/1',
                         '2008/1/1'))
end_dates <- as.Date(c('1992/12/31',
                       '1997/12/31',
                       '2002/12/31',
                       '2007/12/31', 
                       '2012/12/31'))
# start_dates <- as.Date(c('1998/1/1', '2008/1/1'))
# end_dates <- as.Date(c('2002/12/31', '2012/12/31'))
stopifnot(length(start_dates) == length(end_dates))

in_folder <-'I:/Landsat_Originals'
out_base <-'H:/Data/Landsat'
#out_base <-'Z:/Data/Landsat'

included_tiles <- read.csv('Included_tiles.csv')

foreach (sitecode=iter(included_tiles$sitecode),
         wrspath=iter(included_tiles$path),
         wrsrow=iter(included_tiles$row),
         .packages='iterators') %:%
    foreach (start_date=iter(start_dates),
             end_date=iter(end_dates),
             .packages=c('teamlucc')) %dopar% {
        this_pathrow <- sprintf('%03i%03i', wrspath, wrsrow)
        output_folder <- file.path(out_base, sitecode)
        if (!file_test('-d', output_folder)) {
            stop(paste(output_folder, 'does not exist'))
        }
        message(paste0(sitecode, ': ',
                       format(start_date, '%Y/%m/%d'), '-',
                       format(end_date, '%Y/%m/%d'),
                       ', pathrow ', this_pathrow))
        tryCatch(espa_extract(in_folder, output_folder, pathrows=this_pathrow, 
                              start_date=start_date, end_date=end_date),
                 error=function(e) {
                     print(paste0('extract failed for ', sitecode, ' ', 
                                  start_date, '-', end_date))
                 })
}
