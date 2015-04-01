package pap.ass03;

/**
 * Vettore in una viewport grafica
 * 
 * @author aricci
 *
 */
public class V2d  {

    private final int x,y;

    public V2d(int x,int y){
        this.x=x;
        this.y=y;
    }

    public V2d(P2d p1, P2d p0){
    	x = p1.getX() - p0.getX();
    	y = p1.getY() - p0.getY();
    }
    
    public V2d sum(V2d v){
        return new V2d(x+v.x,y+v.y);
    }

    public int getX(){
    	return x;
    }
    
    public int getY(){
    	return y;
    }
    
    public double module(){
        return (double)Math.sqrt(x*x+y*y);
    }

    public String toString(){
        return "("+x+","+y+")";
    }
}
