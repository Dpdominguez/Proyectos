
import jason.asSyntax.*;
import jason.environment.Environment;
import jason.environment.grid.GridWorldModel;
import jason.environment.grid.GridWorldView;
import jason.environment.grid.Location;

import java.util.*;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.util.Random;
import java.util.logging.Logger;



public class Entorno extends Environment {


	public static final int GSize = 5; // grid size
	public static final int OBSTACULO  = 8;//obstaculo
    public static final int STEAK  = 16; // steak code in grid model
	public static final int REDSTEAK  = 32; // steak code in grid model
	public static final int GREENSTEAK  = 64; // steak code in grid model
	public static final int BLACKSTEAK  = 128; // steak code in grid model
	public static final int ORANGESTEAK  = 256; // steak code in grid model
	public static final int MAGENTASTEAK  = 512; // steak code in grid model


    private Logger logger = Logger.getLogger("Entrega2.mas2j."+Entorno.class.getName());

	private Modelo model;
    private Vista  view;

	String label = "  ";
	int own = 0;


    /** Called before the MAS execution with the args informed in .mas2j */

    @Override

    public void init(String[] args) {
		addPercept("judge",Literal.parseLiteral("size("+GSize+")."));
		model = new Modelo();
		view  = new Vista(model);
		model.setView(view);
		///////////////////////////
    super.init(args);
    }

/** creates the agents perception based on the MarsModel */
    void updatePercepts() {
        //Location r1Loc = model.getAgPos(0);
        Literal newMov = Literal.parseLiteral("done");
		addPercept("judge",newMov);
    }

    @Override

    public boolean executeAction(String ag, Structure action) {
			logger.info(ag+" doing: "+ action);
			try {
					 if (ag.equals("judge")) {
			 if (action.getFunctor().equals("intercambiarFichas")) {
				 int Ox = (int)((NumberTerm)action.getTerm(0)).solve();
				 int Oy = (int)((NumberTerm)action.getTerm(1)).solve();
				 int Dx = (int)((NumberTerm)action.getTerm(2)).solve();
				 int Dy = (int)((NumberTerm)action.getTerm(3)).solve();
				 Ficha aux= model.getFicha(Ox,Oy);
				 model.delete(Ox,Oy);
				 Thread.sleep(50);
				 model.colocarFicha(Ox,Oy,model.getFicha(Dx,Dy));
				 Thread.sleep(50);
				 model.delete(Dx,Dy);
				 Thread.sleep(50);
				 model.colocarFicha(Dx,Dy,aux);
				 Thread.sleep(50);

			 }  else if(action.getFunctor().equals("delete")){
				 int x = (int)((NumberTerm)action.getTerm(0)).solve();
				 int y = (int)((NumberTerm)action.getTerm(1)).solve();
				 model.delete(x,y);
			 }			else if(action.getFunctor().equals("put")){
				 int x = (int)((NumberTerm)action.getTerm(0)).solve();
				 int y = (int)((NumberTerm)action.getTerm(1)).solve();
				 int color = (int)((NumberTerm)action.getTerm(2)).solve();
				 String tipo = action.getTerm(3).toString();
				 model.colocarFicha(x,y,new Ficha (traducirColorJuez(color), tipo));
				 
			 }else if(action.getFunctor().equals("nivel")){
				 int x = (int)((NumberTerm)action.getTerm(0)).solve();
				 model.nivel(x);
				 
			 }
		} else {
			logger.info("Recibido una peticion ilegal. "+ag+" no puede realizar la accion: "+ action);
			Literal ilegal = Literal.parseLiteral("accionIlegal(" + ag + ")");
		addPercept("judge",ilegal);
		}
			} catch (Exception e) {
					e.printStackTrace();
			}

			updatePercepts();

			try {
					Thread.sleep(100);
			} catch (Exception e) {}
			informAgsEnvironmentChanged();
			return true;
    }
	
	
	public int traducirColorJuez(int color){
			switch (color){
				case 0: return 16;
				case 1: return 32 ;
				case 2: return 64;
				case 3: return 128 ;
				case 4: return 256 ;
				case 5: return 512 ;
			}
			return 0;
		
		}
			
		public int traducirColorEntorno(int color){
			switch (color){
				case 16: return 0;
				case 32 :return 1;
				case 64: return 2;
				case 128 :return 3;
				case 256 :return 4;
				case 512 :return 5;
			}
		return 0;
		
		}


class Modelo extends GridWorldModel {
		Random random = new Random(System.currentTimeMillis());
		String label = "";
		int nivel;
		Ficha [][] tablero = new Ficha [GSize][GSize];
		int [][]   	duenho = new int [GSize][GSize];
		private Modelo(){
			super(GSize,GSize,2);
			try {
                setAgPos(0, 0, 0);
            } catch (Exception e) {
                e.printStackTrace();
            }
			int color = STEAK;
			for(int x= 0;x < GSize;x++){
				for(int y= 0;y < GSize;y++){
					add(STEAK,x,y);
					tablero[x][y]= new Ficha(color,"in");
					duenho[x][y]= 0;
					addPercept("judge",Literal.parseLiteral("tablero(celda("+x+","+y+","+0+"),ficha("+traducirColorEntorno(color)+","+"in))."));
					if (color < 512) {color = color * 2;}
					else {color = 16;};
				}
			}
			
		}

		void colocarFicha (int x, int y, Ficha ficha){
			if (isFreeOfObstacle(x,y)) {
			 tablero[x][y]= ficha;
					
				set(STEAK,x,y);
			};
		}
		void delete(int x, int y) {
			logger.info("                                                                                                             ");
			remove(STEAK,new Location(x,y));
			if(tablero [x][y]!=null){
				Ficha ficha= tablero [x][y];
			tablero [x][y]= null;
			}

		}
		void nivel(int nivel){
		if(nivel== 2){
			 removePerceptsByUnif("judge",Literal.parseLiteral("tablero(X,Y)"));
			 int color = STEAK;
			for(int x= 0;x < GSize;x++){
				for(int y= 0;y < GSize;y++){
					tablero[x][y]= new Ficha(color,"in");
					duenho[x][y]= 0;
					addPercept("judge",Literal.parseLiteral("tablero(celda("+x+","+y+","+0+"),ficha("+traducirColorEntorno(color)+","+"in))."));
					if (color < 512) {color = color * 2;}
					else {color = 16;};
				}
			}		
			for(int i=0;i<GSize/2;i++){
			int x=random.nextInt(GSize);
			int y=random.nextInt(GSize);
			Ficha ficha= tablero[x][y];
			removePercept("judge",Literal.parseLiteral("tablero(celda("+x+","+y+","+0+"),ficha("+traducirColorEntorno(ficha.getColor())+","+ficha.getTipo()+"))."));
			tablero[x][y].setTipo("obs");
			tablero[x][y].setColor(8);
			addPercept("judge",Literal.parseLiteral("tablero(celda("+x+","+y+","+0+"),obstacle)."));
			set(OBSTACULO,x,y);
			}
		}
		}
		void setDuenho(int o,int x, int y) throws Exception {
			duenho[x][y]=o;
		}
		public int getDuenho(int x, int y) {
			return duenho[x][y];
		}
		public Ficha getFicha (int x,int y){
		return tablero [x][y];
		}
		
		
}// Fin del modelo

class Vista extends GridWorldView {
		String label;
        public Vista(Modelo model) {
            super(model, "Entorno", 600);
            defaultFont = new Font("Arial", Font.BOLD, 18); // change default font
            setVisible(true);
			this.label = model.label;
			//repaint();
        }

        /** draw application objects */
        @Override
        public void draw(Graphics g, int x, int y, int object) {
			if (label == "CO" | label =="PP"){
				logger.info(" la etiqueta que debo dibujar es: "+ label);
			};
			Modelo thisModel = (Modelo) view.getModel();

			String label=thisModel.tablero[x][y].getTipo();
			int color = thisModel.tablero[x][y].getColor();
            switch (object) {
                case Entorno.STEAK: drawSTEAK(g, x, y, color, label);  break;
				case Entorno.OBSTACULO: drawObstaculo(g,x,y);break;
            };

        }

        @Override
        public void drawAgent(Graphics g, int x, int y, Color c, int id) {
            //String label = "R"+(id+1);
            c = Color.white;
            //super.drawAgent(g, x, y, c, -1);
			//drawGarb(g, x, y);
		}

		public void drawSTEAK(Graphics g, int x, int y, int color, String label) {
            switch (color) {
                case Entorno.STEAK: g.setColor(Color.blue);  break;
				case Entorno.REDSTEAK: g.setColor(Color.red);   break;
				case Entorno.GREENSTEAK: g.setColor(Color.green);   break;
				case Entorno.BLACKSTEAK: g.setColor(Color.lightGray);  break;
				case Entorno.ORANGESTEAK: g.setColor(Color.orange);  break;
				case Entorno.MAGENTASTEAK: g.setColor(Color.magenta);   break;
            };
			g.fillOval(x * cellSizeW + 2, y * cellSizeH + 2, cellSizeW - 4, cellSizeH - 4);
			g.setColor(Color.black);
			if(label.equals("in")){
				drawString(g,x,y,defaultFont,"");
			}else{
				drawString(g,x,y,defaultFont,label);
			}
			
		}

        public void drawObstaculo(Graphics g, int x, int y) {
            super.drawObstacle(g, x, y);
            g.setColor(Color.black);
            drawString(g, x, y, defaultFont, "O");
        }

    }//Final Vista

	class Ficha {
		int color;
		String tipo;

		private Ficha (int color, String tipo){
		this.color=color;
		this.tipo=tipo;
		}
		public void setColor (int color){
			this.color=color;
		}
		public void setTipo (String tipo){
			this.tipo=tipo;
		}
		public int getColor (){
			return this.color;
		}
		public String getTipo (){
			return this.tipo;
		}
	}//Fin clase ficha



    /** Called before the end of MAS execution */

    @Override

    public void stop() {

        super.stop();

    }




}
