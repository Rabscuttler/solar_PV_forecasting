
clear;
% x=0:0.1:4;
% y=sin(x);

%%%%%%%% wind_velocity
X0;
X1=0:0.5:9.5;
cftool;

a1 =   134.7  ;
b1 =   0.1457  ;
c1 =   0.3941  ;
a2 =   21.08  ;
b2 =   0.8695  ;
c2 =   -1.369  ;
wind_velocity =  a1*sin(b1*X1+c1) + a2*sin(b2*X1+c2);
wind_velocity=wind_velocity';
S1=[X0  wind_velocity];

%%%%%%%% wind_direction
X2=10:10:360; 
X3;
cftool;

a1 = 121.3;
b1 = 0.006666;
c1 = 0.4417;
a2 = -22.92;
b2 = 0.03946;
c2 = -2.639;
a3 = 27.42 ;
b3 = 0.04855;
c3 = 2.301;  
wind_direction =  a1*sin(b1*X2+c1) + a2*sin(b2*X2+c2) + a3*sin(b3*X2+c3);
wind_direction=wind_direction';
S2=[X3  wind_direction];

