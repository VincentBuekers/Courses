clear all; close all;

%% 1
uiregress

%%
X = ( -3:0.01:3)';
Y = sinc(X) + 0.1.* randn( length(X), 1);

Xtrain = X(1:2: end);
Ytrain = Y(1:2: end);
Xtest = X(2:2: end);
Ytest = Y(2:2: end);

plot(Xtest,Yht)
%%
type='function estimation'

gamlist = [10,1000,1000000];
sig2list = [0.01,1,100];
MSElist = [];

for gam=gamlist
    for sig2=sig2list
        
        perf = crossvalidate({Xtrain , Ytrain , type, gam , sig2, 'RBF_kernel'}, 10 , 'mse');
        MSElist = [MSElist;perf];
        
    end
end

gam = 1000000
sig2=1
[alpha,b] = trainlssvm({Xtrain,Ytrain,type,gam,sig2,'RBF_kernel'});
Yht = simlssvm({Xtrain,Ytrain,type,gam,sig2,'RBF_kernel'}, {alpha,b}, Xtest);
        
figure;
%plotlssvm({Xtrain,Ytrain,type,gam,sig2,'RBF_kernel','preprocess'},{alpha,b});
plot(Xtest,Yht)
hold on
scatter(Xtest,Ytest)
hold off

%% tune

% simplex
iter=10
gamlist=[]
sig2list=[]

for i=1:iter
    [gam, sig2] = tunelssvm({Xtrain, Ytrain , type, [] , [],'RBF_kernel'},...
    'simplex', 'crossvalidatelssvm',{10 , 'mse'});
    gamlist=[gamlist,gam];
    sig2list=[sig2list,sig2];
end
%%
gam_simplex= mean(gamlist)
sig2_simplex= mean(sig2list)
%% 
%gridsearch
iter=10
gamlist=[]
sig2list=[]

for i=1:iter
    [gam, sig2] = tunelssvm({Xtrain, Ytrain , type, [] , [],'RBF_kernel'},...
    'gridsearch', 'crossvalidatelssvm',{10 , 'mse'});
    gamlist=[gamlist,gam];
    sig2list=[sig2list,sig2];
end
%%
gam_grid= mean(gamlist)
sig2_grid= mean(sig2list)

%% Bayesian

sig2 = 0.4;
gam = 10;
crit_L1 = bay_lssvm ({ Xtrain , Ytrain , 'f', gam , sig2 }, 1);
crit_L2 = bay_lssvm ({ Xtrain , Ytrain , 'f', gam , sig2 }, 2);
crit_L3 = bay_lssvm ({ Xtrain , Ytrain , 'f', gam , sig2 }, 3);
[~, alpha ,b] = bay_optimize ({ Xtrain , Ytrain , 'f', gam , sig2 }, 1);
[~, gam] = bay_optimize ({ Xtrain , Ytrain , 'f', gam , sig2 }, 2);
[~, sig2 ] = bay_optimize ({ Xtrain , Ytrain , 'f', gam , sig2 }, 3);
sig2e = bay_errorbar ({ Xtrain , Ytrain , 'f', gam , sig2 }, 'figure');

%% ARD
X = 6.*rand(100 , 3) - 3;
Y = sinc(X(: ,1)) + 0.1.*randn(100 ,1) ;

[ selected , ranking ] = bay_lssvmARD({X, Y, 'f', gam , sig2 });
%%
subplot(1,3,1)
scatter(X(:,1),Y)
title('X_1')
subplot(1,3,2)
scatter(X(:,2),Y)
title('X_2')
subplot(1,3,3)
scatter(X(:,3),Y)
title('X_3')

%% Robust %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

X = ( -6:0.2:6)';
Y = sinc(X) + 0.1.*rand( size (X));

% Outliers can be added via:
out = [15 17 19];
Y( out) = 0.7+0.3*rand( size(out));
out = [41 44 46];
Y( out) = 1.5+0.2*rand( size(out));

%% non-robust model
model = initlssvm(X, Y, 'f', [], [], 'RBF_kernel');
costFun = 'crossvalidatelssvm';
model = tunelssvm(model , 'simplex', costFun , {10 , 'mse';});
model = trainlssvm(model);
plotlssvm(model);
%%

model_rob = initlssvm(X, Y, 'f', [], [], 'RBF_kernel');
costFun = 'rcrossvalidatelssvm';
wFun = 'whuber';
model_rob = tunelssvm(model_rob , 'simplex', costFun , {10 , 'mae';}, wFun );
model_rob = robustlssvm( model_rob );
plotlssvm( model_rob );

%% different loss functions

model = initlssvm(X, Y, 'f', [], [], 'RBF_kernel');
costFun = 'rcrossvalidatelssvm';
wFun = 'whampel';
model = tunelssvm(model , 'simplex', costFun , {10 , 'mae';}, wFun );
model = robustlssvm( model );
plotlssvm( model );
%%
model = initlssvm(X, Y, 'f', [], [], 'RBF_kernel');
costFun = 'rcrossvalidatelssvm';
wFun = 'wlogistic';
model = tunelssvm(model , 'simplex', costFun , {10 , 'mae';}, wFun );
model = robustlssvm( model );
plotlssvm( model );
%%
model = initlssvm(X, Y, 'f', [], [], 'RBF_kernel');
costFun = 'rcrossvalidatelssvm';
wFun = 'wmyriad';
model = tunelssvm(model , 'simplex', costFun , {10 , 'mae';}, wFun );
model = robustlssvm( model );
plotlssvm( model );

%% logmap

clear all; close all;

load logmap.mat
%%
order = 10;
X = windowize (Z, 1:( order + 1));
Y = X(:, end);
X = X(:, 1: order );

gam = 10;
sig2 = 10;
[alpha , b] = trainlssvm ({X, Y, 'f', gam , sig2 });

Xs = Z(end - order +1: end , 1);

nb = 50;
prediction = predict ({X, Y, 'f', gam , sig2 }, Xs , nb);

figure ;
hold on;
plot(Ztest , 'k');
plot(prediction , 'r');
hold off;
%% parameter tuning
orderlist = 10:10:50;

mselist=[];
gamlist = []
sig2list = []

% for each order, tune the gam and sig2 parameters, store MSE
for order=orderlist
    
    X = windowize (Z, 1:( order + 1));
    Y = X(:, end);
    X = X(:, 1: order );

    [gam,sig2,mse] = tunelssvm({X,Y,'f',[],[],'RBF_kernel'},'simplex',...
    'crossvalidatelssvm',{10,'mae'});

    gamlist = [gamlist;gam];
    sig2list = [sig2list;sig2];
    mselist = [mselist;mse];
end

%% retrieve order, gams and sig2 corresponding to best mse
[minimum,index] = min(mselist);
order = orderlist(index);
gam = gamlist(index);
sig2 = sig2list(index);

%% construct training sample with retrieved order
X = windowize(Z, 1:( order+1));
Y = X(:, end);
X = X(:, 1: order );
Xs = Z(end - order: end , 1);

% predict new data (horizon nb)
nb = length(Ztest);
prediction = predict({X, Y, 'f', gam, sig2}, Xs , nb);

k = 1:nb;

figure ;
hold on;
plot(Ztest , 'k');
plot(prediction , 'r');
hold off;

%% Sante fe

clear all;close all;

load santafe.mat;

%%

order = 50;
X = windowize (Z, 1:( order + 1));
Y = X(:, end);
X = X(:, 1: order );

gam = 10;
sig2 = 10;
[alpha , b] = trainlssvm({X, Y, 'f', gam , sig2});

Xs = Z(end - order +1: end , 1);

nb = 200;
prediction = predict ({X, Y, 'f', gam , sig2 }, Xs , nb);

figure ;
hold on;
plot (Ztest , 'k');
plot (prediction , 'r');
hold off;

%%

for lag=10:10:200
    
    %lag = 50;
    Xu = windowize(Z,1:lag+1);
    Xtra = Xu(1:end-lag,1:lag); %training set
    Ytra = Xu(1:end-lag,end); %training set
    Xs = Z(end-lag+1:end,1); %starting point for iterative prediction

    [gam,sig2] = tunelssvm({Xtra,Ytra,'f',[],[],'RBF_kernel'},'simplex',...
    'crossvalidatelssvm',{10,'mae'});

    [alpha,b] = trainlssvm({Xtra,Ytra,'f',gam,sig2,'RBF_kernel'});

    %predict next 100 points
    prediction = predict({Xtra,Ytra,'f',gam,sig2,'RBF_kernel'},Xs,200);
    figure
    plot([prediction Ztest]);
end



%% parameter tuning
orderlist = 100:50:250;

mselist=[];
gamlist = []
sig2list = []

% for each order, tune the gam and sig2 parameters, store MSE
for order=orderlist
    
    X = windowize(Z, 1:(order + 1));
    Y = X(:, end);
    X = X(:, 1: order);
    
    [gam, sig2,mse] = tunelssvm({X, Y , 'f', [] , [],'RBF_kernel'},...
    'simplex', 'crossvalidatelssvm',{10 , 'mse'});

    gamlist = [gamlist;gam];
    sig2list = [sig2list;sig2];
    mselist = [mselist;mse];
end

% retrieve order, gams and sig2 corresponding to best mse
[minimum,index] = min(mselist);
order = orderlist(index);
gam = gamlist(index);
sig2 = sig2list(index);

% construct training sample with retrieved order
X = windowize(Z, 1:( order + 1));
Y = X(:, end);
X = X(:, 1: order );

%% testing
% last observation of training sample
Xs = Z(end - order +1: end , 1);

% predict new data (horizon nb)
nb = length(Ztest);
prediction = predict({X, Y, 'f', gam , sig2 }, Xs , nb);

k = 1:nb;

figure ;
hold on;
scatter(k,Ztest,'*');
plot(prediction , 'r');
hold off;
