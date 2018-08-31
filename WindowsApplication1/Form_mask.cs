using System;
using System.Collections;
using System.Xml;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Diagnostics;

namespace AlkaOpenCell
{
    public partial class Form_mask : Form
    {
        String inputfile;
        String Outputfile;
        XmlDocument xmldoc = new XmlDocument();

        public Form_mask()
        {
            InitializeComponent();
            loadContents();
            
        }

        private void loadContents()
        {            
            xmldoc.Load("C:\\AlkaOpenCell\\src\\settings.xml");
            textBox_S.Text  = xmldoc.GetElementsByTagName("S").Item(0).Attributes[0].Value;
            textBox_PT.Text = xmldoc.GetElementsByTagName("PT").Item(0).Attributes[0].Value;
            textBox_SiT.Text = xmldoc.GetElementsByTagName("SiT").Item(0).Attributes[0].Value;
            textBox_T.Text = xmldoc.GetElementsByTagName("T").Item(0).Attributes[0].Value;
            textBox_W0.Text = xmldoc.GetElementsByTagName("W0").Item(0).Attributes[0].Value;
            textBox_C.Text = xmldoc.GetElementsByTagName("C").Item(0).Attributes[0].Value;
            textBox_DACID.Text = xmldoc.GetElementsByTagName("DACID").Item(0).Attributes[0].Value; 

            
        }

        private void Button_save_Click(object sender, EventArgs e)
        {
            xmldoc.GetElementsByTagName("S").Item(0).Attributes[0].Value = textBox_S.Text;
            xmldoc.GetElementsByTagName("PT").Item(0).Attributes[0].Value = textBox_PT.Text;
            xmldoc.GetElementsByTagName("SiT").Item(0).Attributes[0].Value = textBox_SiT.Text;
            xmldoc.GetElementsByTagName("T").Item(0).Attributes[0].Value = textBox_T.Text;
            xmldoc.GetElementsByTagName("W0").Item(0).Attributes[0].Value = textBox_W0.Text;
            xmldoc.GetElementsByTagName("C").Item(0).Attributes[0].Value = textBox_C.Text;
            xmldoc.GetElementsByTagName("DACID").Item(0).Attributes[0].Value = textBox_DACID.Text; 
            xmldoc.Save("C:\\AlkaOpenCell\\src\\settings.xml"); 

        }


        private void browseButton_Click(object sender, EventArgs e)
        {
            String InitialDirectory, NextTimeInitialDirectory;
            this.labelSuccess.Visible = false;
            this.outputpathlabel.Visible = false;
            OpenFileDialog openFileDialog1 = new OpenFileDialog();
            openFileDialog1.Filter = "Text files | *.txt";
            openFileDialog1.Title = "Select a Cursor File";

            using (StreamReader sr = File.OpenText("C:\\AlkaOpenCell\\src\\path"))
            {
                InitialDirectory = sr.ReadLine();
                sr.Close();
            }

            if (Directory.Exists(InitialDirectory))
                openFileDialog1.InitialDirectory = InitialDirectory;



            // Show the Dialog.
            // If the user clicked OK in the dialog and
            // a .CUR file was selected, open it.
            if (openFileDialog1.ShowDialog() == DialogResult.OK)
            {
                // Assign the cursor in the Stream to the Form's Cursor property.
                //this.Cursor = new Cursor(openFileDialog1.OpenFile());
                inputfile = openFileDialog1.FileName;
                this.labelFilename.Visible = true;
                this.labelFilename.Text = "INPUT:  " + inputfile;
                this.calculateButton.Enabled = true;
                this.CalCulateNoFilterButton.Enabled = true; 

                NextTimeInitialDirectory = Path.GetDirectoryName(inputfile);
                using (StreamWriter sw = File.CreateText("C:\\AlkaOpenCell\\src\\path"))
                {
                    sw.WriteLine(NextTimeInitialDirectory);
                    sw.Close();
                }


            }

        }

        private void CopyFileWithFileName(String pathSRC, String pathDEST)
        {
            String line;
            int i; 
            delete(pathDEST);
            char [] linee = new char[pathDEST.Length];
            for (i = 0; i < pathDEST.Length; i++)
            {
                linee[i]='-'; 
            }        
            StreamWriter sw = File.CreateText(pathDEST);

            sw.WriteLine(linee); 
            sw.WriteLine(pathDEST);
            sw.WriteLine(linee); sw.WriteLine("");

            StreamReader sr = File.OpenText(pathSRC);
            while ((line = sr.ReadLine()) != null)
            {
                sw.WriteLine(line);
            }

            sr.Close(); 
            sw.Close();
        }
        /**
         * 
         */
        private Boolean writeClassicalInputFile() 
        {
            Boolean goodfile = false;
            // LEGGI LA MASK E IL FILE DEL TITOLATORE 
            //CREA input CLASSICO

            String SRC_inputfile = "C:\\AlkaOpenCell\\src\\input";
            
            delete(SRC_inputfile);
                

            StreamWriter sw = File.CreateText(SRC_inputfile);
            sw.WriteLine("SAMPLE CO2 TITRATION DATA");
            sw.WriteLine("");
            sw.WriteLine("S =     " + textBox_S.Text + "      salinity of sample");
            sw.WriteLine("PT =    " + textBox_PT.Text + "        micromol/kg total phosphate");
            sw.WriteLine("SiT =   " + textBox_SiT.Text + "        micromol/kg total silicate");
            sw.WriteLine("");
            sw.WriteLine("T =     " + textBox_T.Text + "      oC temperature of sample when titrated");
            sw.WriteLine("W0 =    " + textBox_W0.Text + "     g  weight of sample titrated");
            sw.WriteLine("C =     " + textBox_C.Text + "    mol/kg concentration of acid titrant");
            sw.WriteLine("DACID=  " + textBox_DACID.Text + "     g/cm3 density of acid titrant");
            sw.WriteLine("");
            sw.WriteLine("V/cm3 E/V");
            sw.WriteLine("------ --------"); 


            double mV; 
            String[] seq; 
            String line = "valore predefinito";
            String newline; 
            StreamReader sr = File.OpenText(inputfile);
            if ((line = sr.ReadLine()) != null)
            {
                if (!line.Contains("Tempo di avvio"))
                {
                    sr.Close();
                    sw.Close();
                    return goodfile;

                }
            }
            



            while ((line = sr.ReadLine()) != null)
            {
                if (line.Contains("oC")) 
                {
                    goodfile = true;
                    break;
                }

            }

            if (!goodfile) {
                sr.Close();
                sw.Close();
                return goodfile; 
            }


            line = sr.ReadLine(); // salto la prima riga di valori
            //posso leggere la matrice
     
            while (( line = sr.ReadLine() ) != null)
            {

                seq = line.Split(null);
                mV = Double.Parse(seq[3])/1000;
                newline = seq[1] + " " + mV.ToString(); 
                sw.WriteLine(newline);
            }
                                   
            sr.Close();
            sw.Close();
            return goodfile;
        }

        private void delete(String pathname)
        {
            if (File.Exists(pathname))
                File.Delete(pathname);
        }

        private Boolean AlkaCalculation()
        {

            if (!writeClassicalInputFile())
            {   this.labelSuccess.Text = " ERROR : Bad File " ; 
                this.labelSuccess.Visible = true;
                return false;
            }


            delete("C:\\AlkaOpenCell\\src\\output");
            Process myProcess = new Process();

            // STEP1 : alka_cellaaperta_noexclusioni.exe legge 'input' scrive 'output'
            myProcess.StartInfo.FileName = "C:\\AlkaOpenCell\\src\\alka_cellaaperta_noexclusioni.exe";
            myProcess.Start();
            myProcess.WaitForExit();

            String theDir = Path.GetDirectoryName(inputfile);
            String FileName = Path.GetFileNameWithoutExtension(inputfile);
            Outputfile = theDir + "\\" + FileName + ".out.txt";

            if (File.Exists("C:\\AlkaOpenCell\\src\\output"))
            {
                this.labelSuccess.Text = "SUCCESSFUL CALCULATION WITHOUT FILTERING. You can see the results in";                
                this.outputpathlabel.Text = Outputfile;
                this.labelSuccess.Visible = true;
                this.outputpathlabel.Visible = true;
            }
            else
            {
                this.labelSuccess.Text = "UNSUCESSFUL CALCULATION";
                this.labelSuccess.Visible = true;
            }

            return true; 
        }


        private void calculateButton_Click(object sender, EventArgs e)
        {

            if (!AlkaCalculation())
            {
                return;
            }

            Process myProcess = new Process();
            // STEP2 : chekdata_pHrange.exe legge 'output' scrive 'new_input'
            Process myProcess2 = new Process();
            myProcess2.StartInfo.FileName = "C:\\AlkaOpenCell\\src\\chekdata_pHrange.exe";
            myProcess2.Start();
            myProcess2.WaitForExit();
                       

            delete("C:\\AlkaOpenCell\\src\\output_stade_1");
            File.Move("C:\\AlkaOpenCell\\src\\output", "C:\\AlkaOpenCell\\src\\output_stade_1" );
            delete("C:\\AlkaOpenCell\\src\\input");
            File.Move("C:\\AlkaOpenCell\\src\\input_new" , "C:\\AlkaOpenCell\\src\\input");
            // STEP3 : alka_cellaaperta_noexclusioni.exe legge 'input' scrive 'output'
            myProcess.StartInfo.FileName = "C:\\AlkaOpenCell\\src\\alka_cellaaperta_noexclusioni.exe";
            myProcess.Start();
            myProcess.WaitForExit();
            

            if (File.Exists("C:\\AlkaOpenCell\\src\\output"))
            {
                this.labelSuccess.Text = "SUCCESSFUL CALCULATION. You can see the filtered results in";
                CopyFileWithFileName("C:\\AlkaOpenCell\\src\\output", Outputfile);
                this.outputpathlabel.Text = Outputfile;
                this.labelSuccess.Visible = true;
                this.outputpathlabel.Visible = true;                
               // System.Diagnostics.Process.Start("notepad.exe", Outputfile);
            }
            else
            {
                this.labelSuccess.Text = "UNSUCESSFUL CALCULATION";
                this.labelSuccess.Visible = true;
            }



        }



        private void CalCulateNoFilterButton_Click(object sender, EventArgs e)
        {

            if (!AlkaCalculation())
            {
                return;
            }

            if (File.Exists("C:\\AlkaOpenCell\\src\\output"))
            {
                CopyFileWithFileName("C:\\AlkaOpenCell\\src\\output", Outputfile);
                //System.Diagnostics.Process.Start("notepad.exe", Outputfile);
            }
        }

        private void exitButton_Click(object sender, EventArgs e)
        {
            this.Close();

        }

        private void help_Click(object sender, EventArgs e)
        {
            Help2Format F = new Help2Format();
            F.ShowDialog(); 
        }

        private void help_Click_1(object sender, EventArgs e)
        {
            Help2 F = new Help2();
            F.ShowDialog(); 
        }



        }
    }
