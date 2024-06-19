package studentConsulting.service.implement;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.mail.internet.MimeMessage;

@Service
public class emailServiceImpl {

    private JavaMailSender javaMailSender;

    @Autowired
    public emailServiceImpl(JavaMailSender javaMailSender)
    {
        this.javaMailSender = javaMailSender;
    }

    @Async
    public void sendEmail(MimeMessage email)
    {
        javaMailSender.send(email);
    }
}
