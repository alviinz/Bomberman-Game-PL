# Bomberman

Bomberman é um jogo simples inspirado no clássico, desenvolvido em Prolog com uma abordagem de programação lógica. O projeto utiliza o SWI-Prolog como interpretador para gerenciar a execução do jogo diretamente no terminal.

## Funcionalidades

* Jogue uma partida de Bomberman em um mapa 2D renderizado no terminal.
* Mova o personagem e plante bombas para destruir caixas.
* Encontre a chave escondida para abrir a porta de saída.
* Vença o jogo escapando pela porta.

## Pré-requisitos

Certifique-se de ter os seguintes requisitos instalados na sua máquina:

* SWI-Prolog (pode ser instalado através do [site oficial](https://www.swi-prolog.org/Download.html))
* Para utilizadores de Windows, é recomendável utilizar o [Subsistema do Windows para Linux (WSL)](https://learn.microsoft.com/pt-br/windows/wsl/install).

## Instalação

1. Clone o repositório do projeto:
   ```bash
   git clone https://github.com/alviinz/Bomberman-Game-PL.git
2. Acesse o diretório do projeto:
   ```bash
   cd Bomberman-Game-PL

3. Instale as dependências do SWI-Prolog(pode ser instalado através do [site oficial](https://www.swi-prolog.org/Download.html))

## Execução

Execute o comando a seguir para iniciar o jogo:
```bash
swipl main.pl
```

## Como jogar

1.  Ao executar a aplicação, o jogo será iniciado diretamente no seu terminal.
2.  Mova o personagem por meio das teclas `W` (Cima), `A` (Esquerda), `S` (Baixo) e `D` (Direita).
3.  Utilize a tecla `Espaço` para plantar bombas e destruir as caixas.
4.  Encontre a Chave escondida sob uma das caixas para poder usar a porta de saída.

## Contribuição

Contribuições são bem-vindas! Se deseja melhorar ou adicionar recursos ao Bomberman, siga estas etapas:

1.  Fork o repositório.
2.  Crie um branch para a sua nova funcionalidade (`git checkout -b feature/nova-funcionalidade`).
3.  Faça as alterações desejadas no código.
4.  Commit as suas alterações (`git commit -am 'Adiciona nova funcionalidade'`).
5.  Push para o branch (`git push origin feature/nova-funcionalidade`).
6.  Abra um Pull Request.
