// This scrip generates the polynomials for the polar curves
//----------------------------------------------------------
Rho=1.225;
Pi =3.14159265358979;

P(:,1)=P(:,1)/3.6;

// Convert to pair of aerodynamic lift and drag coefficients CL-CD
T  =P(:,2)./P(:,1);
E  =atan(T);

CL =(9.8*M*cos(E))./(0.5*Rho*S*(P(:,1)).^2);
CD =CL.*T;

CDi=CL.^2/(Pi*A);
CDp=CD-CDi;

CLCD=CL./CD;
CLCDmax=max(CLCD);

mprintf('Model %s\n', Name);
mprintf('CL/CD_max => %5.1f\n', CLCDmax);
mprintf('CL_max    => %8.6f\n', max(CL));
mprintf('CL_min    => %8.6f\n', min(CL));

scf(0);
clf;
title(Name + " - Polar curves CD(CL)");
plot(CL, CD,  'ro');
plot(CL, CDp, 'go-');
plot(CL, CDi, 'bo-');

// Find the clean polar polynomial
poly_clean=polyfit(CL, CDp, 4);

mprintf('Clean     => %10.8f, %10.8f, %10.8f, %10.8f, %10.8f\n', poly_clean(5), poly_clean(4), poly_clean(3), poly_clean(2), poly_clean(1));

// Reconstruct the polar using the 4th degree polynomial using CL steps
CL_poly_clean= (min(CL):0.005:max(CL));
CD_poly_clean= poly_clean(1)*CL_poly_clean.^4 + poly_clean(2)*CL_poly_clean.^3 + (poly_clean(3)+1/(Pi*A))*CL_poly_clean.^2 + poly_clean(4)*CL_poly_clean + poly_clean(5);
plot(CL_poly_clean, CD_poly_clean, 'm-');

exec rough_polar.sce;

legend(['CD original','CDp clean', 'CDi', 'CD clean','CDp rough'],2);
xs2png(0, Name + '_Polar.png');

// Represent the clean polar in V-W domain for different Mc-values
scf(1);
clf;
title(Name + " - Gliding ratio and optimal speed for sink from 0 to 4 [m/s]");
xlabel("Airspeed [km/h]");
ylabel("Slope");

m=20;

for i=1:m
    
    Sink=4*(i-1)/(m-1);
    
    G=atan(CD_poly_clean./CL_poly_clean);
    
    n=length(G);
    
    Va=zeros(1,n);
    
    for j=1:n
        Va(j)=(2.0 * M * 9.8 * cos(G(j)) / (Rho*S*CL_poly_clean(j))).^0.5;         
    end
    
    Vh=Va.* cos(G);
    Vv=Va.* sin(G);

    H=Vh./(Vv+Sink);
    
    Hmax(i)=0.0;
    Vopt(i)=0.0;
    for j=1:n
        if H(j)>Hmax(i) then
            Hmax(i)=H (j);
            Vopt(i)=3.6*Va(j);
        end
    end
    
    plot(3.6 * Va, H, 'b-');
    
end

plot(Vopt, Hmax, 'm-o');
legend(['Gliding ratio','Speed to fly'],1);
xs2png(1, Name + '_Slope.png')
