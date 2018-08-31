using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;





namespace AlkaOpenCell
{
    public partial class Form1 : Form
    {
        public String inputfile;
        public Form1()
        {
            InitializeComponent();
        }



        private void folderBrowserDialog1_HelpRequest(object sender, EventArgs e)
        {

        }

  
 

        private void button3_Click(object sender, EventArgs e)
        {
            this.Close();
        }






        private void newVersion_Click(object sender, EventArgs e)
        {

            Form_mask F = new Form_mask();
            F.ShowDialog();
        }
 
        private void version1_Click(object sender, EventArgs e)
        {
            Version1 F = new Version1();
            F.ShowDialog(); 
        }





    
    }
}