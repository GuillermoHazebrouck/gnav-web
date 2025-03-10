// This scrip generates the polynomials for the polar curves
//----------------------------------------------------------
clear;
Rho=1.225;
Pi =3.14159265358979;

// Choose here polar to load
exec polar_duo.sce

P(:,1)=P(:,1)/3.6;

// Convert to pair of aerodynamic coefficients
T  =P(:,2)./P(:,1)
E  =atan(T)

CL =(9.8*M*cos(E))./(0.5*Rho*S*(P(:,1)).^2);
CD =CL.*T;

CDi=CL.^2/(Pi*A);
CDp=CD-CDi;

CLCD=CL./CD;
CLCDmax=max(CLCD);

disp(CLCDmax);
disp(max(CL));
disp(min(CL));

scf(0)
clf;
title("Polar curves CD(CL)");
plot(CL, CD,  'ro');
plot(CL, CDp, 'go-');
plot(CL, CDi, 'bo-');

// Find the polynomial
p=polyfit(CL, CDp, 4);
disp(p);

// Reconstruct the polar using the 4th degree polynomial using 0.2 CL steps
CL_poly= (min(CL):0.005:max(CL));
CD_poly= p(1)*CL_poly.^4 + p(2)*CL_poly.^3 + (p(3)+1/(Pi*A))*CL_poly.^2 + p(4)*CL_poly + p(5)
plot(CL_poly, CD_poly, 'm-');

// Represent the polar in V-W domain for different Mc-values
scf(1);
clf;
title("Gliding slope for sink from 0 to 4 [m/s]");
xlabel("Airspeed [km/h]");
ylabel("Slope");

m=20;

for i=1:m
    
    Sink=4*(i-1)/(m-1);
    
    G=atan(CD_poly./CL_poly);
    
    n=length(G);
    
    Va=zeros(1,n);
    
    for j=1:n
        Va(j)=(2.0 * M * 9.8 * cos(G(j)) / (Rho*S*CL_poly(j))).^0.5;         
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
