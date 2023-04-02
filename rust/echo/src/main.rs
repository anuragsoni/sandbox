use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpStream;
use tokio::{io, net::TcpListener};

async fn client_loop(client_socket: &mut TcpStream) -> io::Result<()> {
    let mut buf = vec![0; 1024];
    loop {
        let count = client_socket.read(&mut buf).await?;
        if count == 0 {
            return Ok(());
        }

        client_socket.write_all(&buf[0..count]).await?;
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> io::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:8080").await?;

    loop {
        let (mut client_socket, peer_address) = listener.accept().await?;

        tokio::spawn(async move {
            match client_loop(&mut client_socket).await {
                Ok(()) => (),
                Err(err) => eprintln!("Error while serving client {}. {:?}", peer_address, err),
            }
        });
    }
}
