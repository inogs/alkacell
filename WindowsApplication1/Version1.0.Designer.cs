namespace AlkaOpenCell
{
    partial class Version1
    {
        /// <summary>
        /// Variabile di progettazione necessaria.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Liberare le risorse in uso.
        /// </summary>
        /// <param name="disposing">ha valore true se le risorse gestite devono essere eliminate, false in caso contrario.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Codice generato da Progettazione Windows Form

        /// <summary>
        /// Metodo necessario per il supporto della finestra di progettazione. Non modificare
        /// il contenuto del metodo con l'editor di codice.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Version1));
            this.help = new System.Windows.Forms.Button();
            this.labelSuccess = new System.Windows.Forms.Label();
            this.outputpathlabel = new System.Windows.Forms.Label();
            this.button3 = new System.Windows.Forms.Button();
            this.calculateButton = new System.Windows.Forms.Button();
            this.browseButton = new System.Windows.Forms.Button();
            this.labelFilename = new System.Windows.Forms.Label();
            this.pictureBox1 = new System.Windows.Forms.PictureBox();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
            this.SuspendLayout();
            // 
            // help
            // 
            this.help.Font = new System.Drawing.Font("TechnicBold", 16F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.help.Location = new System.Drawing.Point(590, 115);
            this.help.Name = "help";
            this.help.Size = new System.Drawing.Size(87, 33);
            this.help.TabIndex = 19;
            this.help.Text = "?";
            this.help.UseVisualStyleBackColor = true;
            this.help.Click += new System.EventHandler(this.help_Click);
            // 
            // labelSuccess
            // 
            this.labelSuccess.AutoSize = true;
            this.labelSuccess.Location = new System.Drawing.Point(78, 245);
            this.labelSuccess.Name = "labelSuccess";
            this.labelSuccess.Size = new System.Drawing.Size(275, 13);
            this.labelSuccess.TabIndex = 18;
            this.labelSuccess.Text = "SUCESSFUL CALCULATION. You can see the results in";
            this.labelSuccess.Visible = false;
            // 
            // outputpathlabel
            // 
            this.outputpathlabel.AutoSize = true;
            this.outputpathlabel.Location = new System.Drawing.Point(78, 268);
            this.outputpathlabel.Name = "outputpathlabel";
            this.outputpathlabel.Size = new System.Drawing.Size(61, 13);
            this.outputpathlabel.TabIndex = 17;
            this.outputpathlabel.Text = "output path";
            this.outputpathlabel.Visible = false;
            // 
            // button3
            // 
            this.button3.Location = new System.Drawing.Point(538, 294);
            this.button3.Name = "button3";
            this.button3.Size = new System.Drawing.Size(139, 25);
            this.button3.TabIndex = 16;
            this.button3.Text = "exit";
            this.button3.UseVisualStyleBackColor = true;
            this.button3.Click += new System.EventHandler(this.button3_Click);
            // 
            // calculateButton
            // 
            this.calculateButton.Enabled = false;
            this.calculateButton.Location = new System.Drawing.Point(81, 188);
            this.calculateButton.Name = "calculateButton";
            this.calculateButton.Size = new System.Drawing.Size(286, 39);
            this.calculateButton.TabIndex = 15;
            this.calculateButton.Text = "calculate";
            this.calculateButton.UseVisualStyleBackColor = true;
            this.calculateButton.Click += new System.EventHandler(this.calculateButton_Click);
            // 
            // browseButton
            // 
            this.browseButton.Location = new System.Drawing.Point(81, 115);
            this.browseButton.Name = "browseButton";
            this.browseButton.Size = new System.Drawing.Size(286, 33);
            this.browseButton.TabIndex = 14;
            this.browseButton.Text = "browse for input file";
            this.browseButton.UseVisualStyleBackColor = true;
            this.browseButton.Click += new System.EventHandler(this.browseButton_Click);
            // 
            // labelFilename
            // 
            this.labelFilename.AutoSize = true;
            this.labelFilename.Location = new System.Drawing.Point(78, 163);
            this.labelFilename.Name = "labelFilename";
            this.labelFilename.Size = new System.Drawing.Size(46, 13);
            this.labelFilename.TabIndex = 13;
            this.labelFilename.Text = "filename";
            this.labelFilename.Visible = false;
            // 
            // pictureBox1
            // 
            this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
            this.pictureBox1.Location = new System.Drawing.Point(0, -3);
            this.pictureBox1.Name = "pictureBox1";
            this.pictureBox1.Size = new System.Drawing.Size(744, 103);
            this.pictureBox1.TabIndex = 34;
            this.pictureBox1.TabStop = false;
            // 
            // Version1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(744, 331);
            this.Controls.Add(this.pictureBox1);
            this.Controls.Add(this.help);
            this.Controls.Add(this.labelSuccess);
            this.Controls.Add(this.outputpathlabel);
            this.Controls.Add(this.button3);
            this.Controls.Add(this.calculateButton);
            this.Controls.Add(this.browseButton);
            this.Controls.Add(this.labelFilename);
            this.Name = "Version1";
            this.Text = "AlkaOpenCell 1.0";
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button help;
        private System.Windows.Forms.Label labelSuccess;
        private System.Windows.Forms.Label outputpathlabel;
        private System.Windows.Forms.Button button3;
        private System.Windows.Forms.Button calculateButton;
        private System.Windows.Forms.Button browseButton;
        private System.Windows.Forms.Label labelFilename;
        private System.Windows.Forms.PictureBox pictureBox1;
    }
}