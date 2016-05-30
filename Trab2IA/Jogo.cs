using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using SbsSW.SwiPlCs;
using SbsSW.SwiPlCs.Streams;

namespace Trab2IA
{

    public partial class Jogo : Form
    {
        #region Variaveis

        #region Configs
        private const int TempoDeEspera = 400;//90
        private const int TamanhoDoMapa = 12;
        private const int TamanhoDoQuadrado = 60;
        private const string NomeProlog = "Prolog.pl";
        private static readonly string Caminho = Path.GetFullPath(Path.Combine(Directory.GetCurrentDirectory(), @"..\..\..")); // 3 levels up
        private static readonly string CaminhoProlog = Caminho + "/Prolog/" + NomeProlog;
        #endregion

        #region Imagens
        private SpriteAnim _marceline;
        private SpriteAnim _jake;
        private SpriteAnim _reigelado;
        private Bitmap _poco;
        private SpriteAnim _moeda;
        private Bitmap _terra;
        private Bitmap _entrada;
        private SpriteAnim _jujuba;
        private Bitmap _powerup;
        #endregion

        #region Prolog Utils
        private string[] _arquivo;
        private string[] _param;
        private Dictionary<char, object> _imagensVsCasa;
        private int _posicaoJjbX;
        private int _posicaoJjbY = 11;
        private bool _vivo;
        private int _pontuacao;
        private int _energia;
        private int _ouros;
        private string _melhoracao;
        private string _orientacao;

        #endregion

        #endregion

        public Jogo()
        {
            InitializeComponent();
            Programa();
        }

        private void Programa()
        {
            CarregaImagens();
            CarregaMapaEscolhido();
            Configuracoes();
            Prolog();
        }

        #region Inicialização
        private void CarregaImagens()
        {
            var pathImagem = Caminho + "/imagens/";
            _marceline = new SpriteAnim(pathImagem + "animMarcy.png", 8);
            _jake = new SpriteAnim(pathImagem + "animJake.png", 8);
            _reigelado = new SpriteAnim(pathImagem + "animReiGelado.png", 8);
            _poco = new Bitmap(pathImagem + "poco.png");
            _moeda = new SpriteAnim(pathImagem + "animMoeda.png", 8);
            _terra = new Bitmap(pathImagem + "terra.png");
            _entrada = new Bitmap(pathImagem + "entrada.png");
            _jujuba = new SpriteAnim(pathImagem + "animJujuba.png", 8);
            _powerup = new Bitmap(pathImagem + "powerup.png");
        }

        private void CarregaMapaEscolhido()
        {
            var caminhoArquivo = "";
            using (FileDialog fileDialog = new OpenFileDialog())
            {
                if (fileDialog.ShowDialog(this) == DialogResult.OK)
                    caminhoArquivo = fileDialog.InitialDirectory + fileDialog.FileName;
            }
            _arquivo = File.ReadAllLines(caminhoArquivo);
        }

        private void Configuracoes()
        {
            _imagensVsCasa = new Dictionary<char, object>
            {
                {'X', _entrada},
                {'D', _marceline},
                {'T', _jake},
                {'d', _reigelado},
                {'P', _poco},
                {'U', _powerup},
                {'O', _moeda},
                {'.', _terra}
            };
            _param = new[]{ "-q", "-f", CaminhoProlog };
        }
        #endregion

        #region Prolog

        private void Prolog()
        {
            CheckForIllegalCrossThreadCalls = false;
            _vivo = true;
            Task.Run(() =>
            {
                while (_vivo)
                {
                    try
                    {
                        RodaProlog();
                        //TesteJujubaAndando();
                    }
                    catch (Exception ex)
                    {
                        Close();
                    }
                }
                PlEngine.PlCleanup();
            });
        }
        
        private void RodaProlog()
        {
            if (!PlEngine.IsInitialized)
                PlEngine.Initialize(_param);
            int posicaoY = 0, posicaoX = 0;
            using (PlQuery query = new PlQuery("estado(casa(X,Y),W,P,E,O,V)."))
            {
                foreach (PlQueryVariables vars in query.SolutionVariables)
                {
                    posicaoY = (int) vars[query.VariableNames[0]] - 1; //linha
                    posicaoX = (int) vars[query.VariableNames[1]] - 1; //coluna
                    _orientacao = vars[query.VariableNames[2]].ToString();
                    _pontuacao = (int) vars[query.VariableNames[3]];
                    _energia = (int) vars[query.VariableNames[4]];
                    _ouros = (int) vars[query.VariableNames[5]];
                    _vivo = vars[query.VariableNames[6]].ToString() == "vivo";
                }
            }
            /*
            if (posicaoY == _posicaoJjbY)
            {
                if (posicaoX == _posicaoJjbX + 1 || posicaoX == _posicaoJjbX - 1)
                {
                    _posicaoJjbY = posicaoY;
                    _posicaoJjbX = posicaoX;
                    Refresh();
                }
                else
                {
                    var tempX = _posicaoJjbX;
                    _posicaoJjbY = posicaoY;
                    if (posicaoX > _posicaoJjbX)
                        for (var i = tempX; i <= posicaoX; i++)
                        {
                            _posicaoJjbX = i;
                            Refresh();
                        }
                    else
                        for (var i = posicaoX; i <= tempX; i++)
                        {
                            _posicaoJjbX = i;
                            Refresh();
                        }
                }
            }
            else
            {
                if (posicaoX == _posicaoJjbX)
                    if (posicaoY == _posicaoJjbY + 1 || posicaoY == _posicaoJjbY - 1)
                    {
                        _posicaoJjbY = posicaoY;
                        _posicaoJjbX = posicaoX;
                        Refresh();
                    }
                    else
                    {
                        var tempY = _posicaoJjbY;
                        _posicaoJjbX = posicaoX;
                        if (posicaoX > _posicaoJjbX)
                            for (var i = tempY; i <= posicaoY; i++)
                            {
                                _posicaoJjbY = i;
                                Refresh();
                            }
                        else
                            for (var i = posicaoY; i <= tempY; i++)
                            {
                                _posicaoJjbY = i;
                                Refresh();
                            }
                    }
                else
                {
                    _posicaoJjbX = posicaoX;
                    _posicaoJjbY = posicaoY;
                    Refresh();
                }
            }*/
            _posicaoJjbX = posicaoX;
            _posicaoJjbY = posicaoY;
            Refresh();
            Thread.Sleep(TempoDeEspera);
            var melhoracao = PlQuery.PlCallQuery("melhor_acao(X)");
            _melhoracao = melhoracao.Name;
        }

        private void TesteJujubaAndando()
        {
            for (var i = 11; i > 1; i--)
            {
                _posicaoJjbY = i;
                _posicaoJjbX = 0;
                Refresh();
                Thread.Sleep(TempoDeEspera);
            }
        }

        #endregion

        #region Interface
        private void PrintaInterface(object sender, PaintEventArgs e)
        {
           DesenhaMapa(e);
           DesenhaJujuba(e);
           DesenhaLinhasDoMapa(e);
           EscreveInformacoes(e);
           GraphicsDrawSpriteAnim.Frame++;
        }

       
        private void DesenhaMapa(PaintEventArgs e)
        {
            for (var j = 0; j < TamanhoDoMapa; j++)
                for (var i = 0; i < TamanhoDoMapa; i++)
                {
                    if (_imagensVsCasa[_arquivo[j][i]] is Bitmap)
                        e.Graphics.DrawImage((Bitmap) _imagensVsCasa[_arquivo[j][i]], TamanhoDoQuadrado*i, TamanhoDoQuadrado*j,
                            TamanhoDoQuadrado, TamanhoDoQuadrado);
                    else
                        e.Graphics.DrawSpriteAnim((SpriteAnim) _imagensVsCasa[_arquivo[j][i]], TamanhoDoQuadrado*i,
                            TamanhoDoQuadrado*j,
                            TamanhoDoQuadrado, TamanhoDoQuadrado);
                }
        }

        private void DesenhaJujuba(PaintEventArgs e)
        {
            var objetoNaCasaAtual = _arquivo[_posicaoJjbY][_posicaoJjbX];
            switch (objetoNaCasaAtual)
            {
                case 'D': //inimigo morreu? tira do mata
                    break;
                case 'd': //inimigo morreu? tira do mata
                    break;
                case 'T': //inimigo morreu? tira do mata
                    break;
                case 'O': //checar se realizou a ação pegar - se sim, tira moeda do mapa
                    if (_melhoracao == "pegar_ouro")
                        FazerTerrenoVirarTerra();
                    break;
                case 'U': //checar se realizou a ação pegar - se sim, tira o powerup do mapa
                    if (_melhoracao == "pegar_vida")
                        FazerTerrenoVirarTerra();
                    break;
            }
            e.Graphics.DrawSpriteAnim(_jujuba, TamanhoDoQuadrado * _posicaoJjbX, TamanhoDoQuadrado * _posicaoJjbY,
                             TamanhoDoQuadrado, TamanhoDoQuadrado);
        }

        private void FazerTerrenoVirarTerra()
        {
            var parts = new char[_arquivo[_posicaoJjbY].Length];
            for (var i = 0; i < parts.Length; i++)
            {
                if (i == _posicaoJjbX)
                    parts[i] = '.';
                else
                    parts[i] = _arquivo[_posicaoJjbY][i];
            }
            _arquivo[_posicaoJjbY] = new string(parts);
        }

        private void DesenhaLinhasDoMapa(PaintEventArgs e)
        {
            var linha = new Pen(Brushes.DarkOliveGreen);
            var action = new Action<int>[]
            {
                i => e.Graphics.DrawLine(linha, i, 0, i, TamanhoDoQuadrado*TamanhoDoMapa),
                i => e.Graphics.DrawLine(linha, 0, i, TamanhoDoQuadrado*TamanhoDoMapa, i)
            };
            foreach (var printLineDirectionMethod in action)
                for (var i = 1; i < TamanhoDoMapa; i++)
                    printLineDirectionMethod(i*TamanhoDoQuadrado);
        }

       private void EscreveInformacoes(PaintEventArgs e)
       {
           e.Graphics.DrawString("Pontuacao: " + _pontuacao, new Font(FontFamily.GenericSerif, 20, FontStyle.Bold), Brushes.Indigo, new PointF(0, 0));
           e.Graphics.DrawString("Energia: " + _energia, new Font(FontFamily.GenericSerif, 20, FontStyle.Bold), Brushes.Indigo, new PointF(300, 0));
           e.Graphics.DrawString("Ouros: " + _ouros, new Font(FontFamily.GenericSerif, 20, FontStyle.Bold), Brushes.Indigo, new PointF(600, 0));
           e.Graphics.DrawString("Melhor Ação: " + _melhoracao, new Font(FontFamily.GenericSerif, 20, FontStyle.Bold), Brushes.Indigo, new PointF(200, 660));
       }
        #endregion

    }
}
