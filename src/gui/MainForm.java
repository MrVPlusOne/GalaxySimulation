package gui;

import gui.MainFormScala;

import javax.swing.*;

/**
 * Created by weijiayi on 8/15/15.
 */
public class MainForm {
    public JTextField timeField;
    public JTextField intervalField;
    public JPanel canvasHolder;
    public JPanel top;
    public JButton simButton;
    public JSlider frameSlider;
    public JLabel frameLabel;
    public JTextField centerXField;
    public JTextField centerYField;
    public JTextField viewSizeField;
    public JTextField seedField;
    public JLabel speedLabel;
    public JTextField simplifyField;
    public JCheckBox showGridBox;
    public JCheckBox showCalculationBox;

    public static void main(String[] args) {
        JFrame frame = new JFrame("Mr Meow's Galaxy Simulation");
        MainForm mainForm = new MainForm();

        new MainFormScala(mainForm);

        frame.setContentPane(mainForm.top);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }
}
