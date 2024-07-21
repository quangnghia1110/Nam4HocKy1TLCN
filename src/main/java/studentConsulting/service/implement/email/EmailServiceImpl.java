package studentConsulting.service.implement.email;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.mail.internet.MimeMessage;

@Service
public class EmailServiceImpl {
    @Autowired

    private JavaMailSender javaMailSender;
    public EmailServiceImpl(JavaMailSender javaMailSender)
    {
        this.javaMailSender = javaMailSender;
    }

    @Async
    public void sendEmail(MimeMessage email)
    {
        javaMailSender.send(email);
    }
}
