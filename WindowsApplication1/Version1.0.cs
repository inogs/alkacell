using System;
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
    public partial class Version1 : Form
    {
        public String inputfile;
        public Version1()
        {
            InitializeComponent();
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

                NextTimeInitialDirectory = Path.GetDirectoryName(inputfile);
                using (StreamWriter sw = File.CreateText("C:\\AlkaOpenCell\\src\\path"))
                {
                    sw.WriteLine(NextTimeInitialDirectory);
                    sw.Close();
                }


            }

        }

        private void calculateButton_Click(object sender, EventArgs e)
        {
            File.Copy(inputfile, "C:\\AlkaOpenCell\\src\\input", true);

            if (File.Exists("C:\\AlkaOpenCell\\src\\output"))
                File.Delete("C:\\AlkaOpenCell\\src\\output");

            Process myProcess = new Process();
            myProcess.StartInfo.FileName = "C:\\AlkaOpenCell\\src\\alka_cellaaperta_noexclusioni.exe";
            myProcess.Start();
            myProcess.WaitForExit();



            String theDir = Path.GetDirectoryName(inputfile);
            String FileName = Path.GetFileNameWithoutExtension(inputfile);
            String Outputfile = theDir + "\\" + FileName + ".out.txt";

            if (File.Exists("C:\\AlkaOpenCell\\src\\output"))
            {
                this.labelSuccess.Text = "SUCESSFUL CALCULATION. You can see the results in";
                File.Copy("C:\\AlkaOpenCell\\src\\output", Outputfile, true);
                this.outputpathlabel.Text = Outputfile;
                this.labelSuccess.Visible = true;
                this.outputpathlabel.Visible = true;
            }
            else
            {
                this.labelSuccess.Text = "UNSUCESSFUL CALCULATION";
                this.labelSuccess.Visible = true;
            }
        }

        private void help_Click(object sender, EventArgs e)
        {
            Form2 F = new Form2();
            F.ShowDialog();
        }

        private void button3_Click(object sender, EventArgs e)
        {
            this.Close(); 
        }
    }
}