clear all; close all; clc;
%% 1
X1 = randn(50,2) + 1;
X2 = randn(51,2) - 1;

Y1 = ones(50);
Y2 = -ones(51);

X = [4,-4];
Y = [-4,4];

figure ;
hold on;
plot(X1(: ,1) , X1(: ,2) , 'ro');
plot(X2(: ,1) , X2(: ,2) , 'bo');
line(X,Y, 'color','black');
hold off;

%% 2
load iris.mat
%%
% Train the LS-SVM classifier using polynomial kernel
%
type='c'; 
gam = 1; 
t = 1; 
degreelist = [1,2,3,4,5];

for degree=degreelist,
    disp('Polynomial kernel of varying degrees'),

    [alpha,b] = trainlssvm({Xtrain,Ytrain,type,gam,[t; degree],'poly_kernel'});

    figure; plotlssvm({Xtrain,Ytrain,type,gam,[t; degree],'poly_kernel','preprocess'},{alpha,b});

    [Yht, Zt] = simlssvm({Xtrain,Ytrain,type,gam,[t; degree],'poly_kernel'}, {alpha,b}, Xtest);

    err = sum(Yht~=Ytest); 
    fprintf('\n on test: #misclass = %d, error rate = %.2f%%\n', err, err/length(Ytest)*100)
    disp('Press any key to continue...'), pause,        
end  
%% 3
%
% use RBF kernel
%

% tune the sig2 while fix gam
%
disp('RBF kernel')
gam = 1; sig2list=[0.001:0.05:20];

errlist=[];

for sig2=sig2list,
    disp(['gam : ', num2str(gam), '   sig2 : ', num2str(sig2)]),
    [alpha,b] = trainlssvm({Xtrain,Ytrain,type,gam,sig2,'RBF_kernel'});
    
    % Plot the decision boundary of a 2-d LS-SVM classifier
    %plotlssvm({Xtrain,Ytrain,type,gam,sig2,'RBF_kernel','preprocess'},{alpha,b});

    % Obtain the output of the trained classifier
    [Yht, Zt] = simlssvm({Xtrain,Ytrain,type,gam,sig2,'RBF_kernel'}, {alpha,b}, Xtest);
    err = sum(Yht~=Ytest); errlist=[errlist; err];         
end


%
% make a plot of the misclassification rate wrt. sig2

figure;
plot(log(sig2list), errlist, '*-'), 
xlabel('log(sig2)'), ylabel('number of misclass'),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4
% fix sigma at 1
errlist=[];
gamlist = [0.001:0.01:10]; 
sig2=1;

for gam=gamlist,
    disp(['gam : ', num2str(gam), '   sig2 : ', num2str(sig2)]),
    [alpha,b] = trainlssvm({Xtrain,Ytrain,type,gam,sig2,'RBF_kernel'});

    % Plot the decision boundary of a 2-d LS-SVM classifier
    %plotlssvm({Xtrain,Ytrain,type,gam,sig2,'RBF_kernel','preprocess'},{alpha,b});

    % Obtain the output of the trained classifier
    [Yht, Zt] = simlssvm({Xtrain,Ytrain,type,gam,sig2,'RBF_kernel'}, {alpha,b}, Xtest);
    err = sum(Yht~=Ytest); errlist=[errlist; err];        
end

figure;
plot(log(gamlist), errlist, '*-'), 
xlabel('log(gam)'), ylabel('number of misclass'),

%% parameter tuning
perflist_val=[];

gamlist = [0.001:100:1000];
sig2list = [0.001:100:1000]; 
[X,Y] = meshgrid(gamlist,sig2list);

for gam=gamlist,
    for sig2=sig2list,
        perf = rsplitvalidate({Xtrain , Ytrain , 'c', gam , sig2, 'RBF_kernel'}, 0.80 , 'misclass');
        perflist_val=[perflist_val; perf];
    end
end

perflist_val = reshape(perflist_val,[10,10]);

% K-fold
perflist_cross=[]; 

for gam=gamlist,
    for sig2=sig2list,
        perf = crossvalidate({Xtrain , Ytrain , 'c', gam , sig2, 'RBF_kernel'}, 10 , 'misclass');
        perflist_cross=[perflist_cross; perf];
    end
end

perflist_cross = reshape(perflist_cross,[10,10]);

% leavoneout
perflist_loo=[]; 

for gam=gamlist,
    for sig2=sig2list,
        perf = leaveoneout({Xtrain , Ytrain , 'c', gam , sig2, 'RBF_kernel'}, 'misclass');
        perflist_loo=[perflist_loo; perf];
    end
end

perflist_loo = reshape(perflist_loo,[10,10]);

%%

subplot(1,3,1)
surf(X,Y, perflist_val), 
xlabel('Gamma'), ylabel('Sigma2'),zlabel('# misclassifications')
title('Random Split')

subplot(1,3,2)
surf(X,Y, perflist_cross), 
xlabel('Gamma'), ylabel('Sigma2'),zlabel('# misclassifications')
title('10-fold Crossvalidation')

subplot(1,3,3)
surf(X,Y, perflist_loo), 
xlabel('Gamma'), ylabel('Sigma2'),zlabel('# misclassifications')
title('Leave-one-out')

%% ARD
% simplex
[gam, sig2, cost] = tunelssvm({Xtrain, Ytrain , 'c', [] , [],'RBF_kernel'},...
    'simplex', 'crossvalidatelssvm',{10 , 'misclass'});
%% 
%gridsearch
[gam, sig2, cost] = tunelssvm({Xtrain, Ytrain , 'c', [] , [],'RBF_kernel'},...
    'gridsearch', 'crossvalidatelssvm',{10 , 'misclass'});
%% ROC

% Train the classification model.
[ alpha , b ] = trainlssvm({Xtrain , Ytrain , 'c', gam , sig2 ,'RBF_kernel'});
% Classification of the test data.
[ Yest , Ylatent ] = simlssvm({Xtrain , Ytrain , 'c', gam , sig2 ,'RBF_kernel'} , {alpha , b} , Xtest);
% Generating the ROC curve.
roc(Ylatent, Ytest);

%% Bayesian

bay_modoutClass({ Xtrain , Ytrain , 'c', 100 , 100 } , 'figure');

%% Ripley
clear all;close all;

load ripley.mat
%% visualize

 
figure ;
gscatter(Xtrain(: ,1) , Xtrain(: ,2) , Ytrain,'br','xo');
gscatter(Xtrain(: ,1) , Xtrain(: ,2) , Ytrain,'br','xo');

%% 
%gridsearch
[gam, sig2, cost] = tunelssvm({Xtrain, Ytrain , 'c', [] , [],'RBF_kernel'},...
    'gridsearch', 'crossvalidatelssvm',{10 , 'misclass'});

%% ROC
% Train the classification model.
[ alpha , b ] = trainlssvm({Xtrain , Ytrain , 'c', gam , sig2 ,'RBF_kernel'});
figure; plotlssvm({Xtrain,Ytrain,'c',gam,sig2,'RBF_kernel','preprocess'},{alpha,b});

% Classification of the test data.
[ Yest , Ylatent ] = simlssvm({Xtrain , Ytrain , 'c', gam , sig2 ,'RBF_kernel'} , {alpha , b} , Xtest);
% Generating the ROC curve.
roc(Ylatent, Ytest);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%gridsearch
[gam, t, degree] = tunelssvm({Xtrain, Ytrain , 'c', [] , [],'poly_kernel'},...
    'gridsearch', 'crossvalidatelssvm',{10 , 'misclass'});

%% ROC
% Train the classification model.
[ alpha , b ] = trainlssvm({Xtrain , Ytrain , 'c', gam , [t(1),t(2)] ,'poly_kernel'});
figure; plotlssvm({Xtrain,Ytrain,'c',gam,[t(1),t(2)],'poly_kernel','preprocess'},{alpha,b});

% Classification of the test data.
[ Yest , Ylatent ] = simlssvm({Xtrain , Ytrain , 'c', gam , sig2 ,'RBF_kernel'} , {alpha , b} , Xtest);
% Generating the ROC curve.
roc(Ylatent, Ytest);

%% Wisconsin
clear all;close all;

load breast.mat
%% visualize

figure;
embedding = tsne(trainset);
embedding_test = tsne(testset);
gscatter(embedding(:,1),embedding(:,2),labels_train)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RBF

% gridsearch
[gam, sig2] = tunelssvm({trainset, labels_train , 'c', [] , [],'RBF_kernel'},...
    'gridsearch', 'crossvalidatelssvm',{10 , 'misclass'});

% Train the classification model.
[ alpha , b ] = trainlssvm({trainset , labels_train , 'c', gam , sig2 ,'RBF_kernel'});
figure; plotlssvm({trainset,labels_train,'c',gam,sig2,'RBF_kernel','preprocess'},{alpha,b});

% Classification of the test data.
[ Yest , Ylatent_rbf ] = simlssvm({trainset , labels_train , 'c', gam , sig2 ,'RBF_kernel'}...
    , {alpha , b} , testset);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% linear kernel
%gridsearch
gam = tunelssvm({trainset, labels_train , 'c', [] , [],'lin_kernel'},...
    'gridsearch', 'crossvalidatelssvm',{10 , 'misclass'});

% Train the classification model.
[ alpha , b ] = trainlssvm({trainset , labels_train , 'c', gam , [] ,'lin_kernel'});
%figure; plotlssvm({embedding,labels_train,'c',gam,[],'lin_kernel','preprocess'},{alpha,b});

% Classification of the test data.
[ Yest , Ylatent_lin ] = simlssvm({trainset , labels_train , 'c', gam , [] ,'lin_kernel'}...
    , {alpha , b} , testset);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% poly kernel

%gridsearch
[gam, t, degree] = tunelssvm({trainset, labels_train , 'c', [] , [],'poly_kernel'},...
    'gridsearch', 'crossvalidatelssvm',{10 , 'misclass'});

% Train the classification model.
[ alpha , b ] = trainlssvm({trainset , labels_train , 'c', gam , [t(1),t(2)] ,'poly_kernel'});
%figure; plotlssvm({embedding,labels_train,'c',gam,[t(1),t(2)],'poly_kernel','preprocess'},{alpha,b});

% Classification of the test data.
[ Yest , Ylatent_poly ] = simlssvm({trainset , labels_train , 'c', gam , [t(1),t(2)] ,'poly_kernel'}...
    , {alpha , b} , testset);

%% Generating the ROC curves.

roc(Ylatent_rbf, labels_test);
roc(Ylatent_lin, labels_test);
roc(Ylatent_poly, labels_test);

%% diabetes
clear all;close all;

load diabetes.mat
%% visualize


[W,H] = nnmf(trainset,2);

biplot(H','scores',W, 'VarLabels', {'1','2','3','4','5','6','7','8','9','10',...
    '11','12','13','14','15','16','17','18','19','20','21','22','23','24','25',...
    '26','27','28','29','30'});
axis([0 1.1 0 1.1])
xlabel('Column 1')
ylabel('Column 2')

figure ;
gscatter(trainset(: ,4) , trainset(: ,24) , labels_train,'br','xo');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RBF

% gridsearch
[gam, sig2] = tunelssvm({trainset(:,[4,24]), labels_train , 'c', [] , [],'RBF_kernel'},...
    'gridsearch', 'crossvalidatelssvm',{10 , 'misclass'});

% Train the classification model.
[ alpha , b ] = trainlssvm({trainset(:,[4,24]) , labels_train , 'c', gam , sig2 ,'RBF_kernel'});
figure; plotlssvm({trainset(:,[4,24]),labels_train,'c',gam,sig2,'RBF_kernel','preprocess'},{alpha,b});

% Classification of the test data.
[ Yest , Ylatent_rbf ] = simlssvm({trainset(:,[4,24]) , labels_train , 'c', gam , sig2 ,'RBF_kernel'} , {alpha , b} , testset(:,[4,24]));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% linear kernel
%gridsearch
gam = tunelssvm({trainset(:,[4,24]), labels_train , 'c', [] , [],'lin_kernel'},...
    'gridsearch', 'crossvalidatelssvm',{10 , 'misclass'});

% Train the classification model.
[ alpha , b ] = trainlssvm({trainset(:,[4,24]) , labels_train , 'c', gam , sig2 ,'lin_kernel'});
figure; plotlssvm({trainset(:,[4,24]),labels_train,'c',gam,sig2,'lin_kernel','preprocess'},{alpha,b});

% Classification of the test data.
[ Yest , Ylatent_lin ] = simlssvm({trainset(:,[4,24]) , labels_train , 'c', gam , sig2 ,'lin_kernel'} , {alpha , b} , testset(:,[4,24]));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% poly kernel

%gridsearch
[gam, t, degree] = tunelssvm({trainset(:,[4,24]), labels_train , 'c', [] , [],'poly_kernel'},...
    'gridsearch', 'crossvalidatelssvm',{10 , 'misclass'});

% Train the classification model.
[ alpha , b ] = trainlssvm({trainset(:,[4,24]) , labels_train , 'c', gam , [t(1),t(2)] ,'poly_kernel'});
% Classification of the test data.
[ Yest , Ylatent_poly ] = simlssvm({trainset(:,[4,24]) , labels_train , 'c', gam , sig2 ,'RBF_kernel'} , {alpha , b} , testset(:,[4,24]));

%% Generating the ROC curve.

roc(Ylatent_rbf, labels_test);
roc(Ylatent_lin, labels_test);
roc(Ylatent_poly, labels_test);
