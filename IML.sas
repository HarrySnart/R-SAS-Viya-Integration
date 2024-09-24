/* plot iris dataset with species as colour */
proc sgplot data=sashelp.iris;
STYLEATTRS DATACOLORS=(LIGHTGREEN YELLOW ROSE);
scatter x=petalwidth y=petallength  / group=species filledoutlinedmarkers markerattrs=(symbol=CircleFilled size=14 ) ;
title 'Iris Data with Species Label';
run;

/* check R is enabled */
proc options option=rlang;run;

proc iml;
/* read sas dataset into IML */
use Sashelp.iris;
read all var {sepallength sepalwidth petallength petalwidth} into iris[colname = varNames];

/* print data */
print iris;

/* export data to R matrix */
call ExportMatrixToR(iris,"r_iris");

/* run R code on dataset */
submit / r;
# kmeans clustering
kmeans_result <- kmeans(r_iris, centers = 3, nstart = 20)
cluster <- as.numeric(as.factor(kmeans_result$cluster))
# append column vector to original matrix
cluster_out <- cbind(r_iris,cluster)
endsubmit;

/* export data back to IML from R */
call ImportMatrixFromR(cluster_out,"cluster_out");


/* print results */
print cluster_out;

varNames = {
"SepalLength","SepalWidth","PetalLength","PetalWidth","Cluster"
};

/* save results to SAS dataset with variable names */
create clusters from cluster_out[colname=varNames];
append from cluster_out;
close clusters;

quit;

ods graphics / noborder;

/* plot iris dataset with assigned cluster as colour */
proc sgplot data=clusters;
STYLEATTRS DATACOLORS=(LIGHTGREEN YELLOW ROSE);
scatter x=petalwidth y=petallength  / group=cluster filledoutlinedmarkers markerattrs=(symbol=CircleFilled size=14 ) ;
title 'Iris Data with Clusters from R';
run;
