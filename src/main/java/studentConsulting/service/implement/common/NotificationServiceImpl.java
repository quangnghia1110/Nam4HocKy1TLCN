package studentConsulting.service.implement.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.ConsultationScheduleEntity;
import studentConsulting.model.entity.ConsultationScheduleRegistrationEntity;
import studentConsulting.model.entity.NotificationEntity;
import studentConsulting.model.payload.dto.common.NotificationResponseDTO;
import studentConsulting.model.payload.dto.common.NotificationResponseDTO.NotificationDTO;
import studentConsulting.repository.actor.ConsultationScheduleRegistrationRepository;
import studentConsulting.repository.actor.ConsultationScheduleRepository;
import studentConsulting.repository.common.NotificationRepository;
import studentConsulting.security.config.Email.EmailService;
import studentConsulting.service.interfaces.common.INotificationService;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.List;

@Service
public class NotificationServiceImpl implements INotificationService {

    private final NotificationRepository notificationRepository;
    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @Autowired
    private ConsultationScheduleRegistrationRepository registrationRepository;

    @Autowired
    private EmailService emailService;

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private JavaMailSender javaMailSender;

    @Autowired
    public NotificationServiceImpl(NotificationRepository notificationRepository) {
        this.notificationRepository = notificationRepository;
    }

    @Override
    public void sendUserNotification(Integer senderId, Integer receiverId, String content, NotificationType type) {
        NotificationEntity notification = NotificationEntity.builder()
                .senderId(senderId)
                .receiverId(receiverId)
                .content(content)
                .time(LocalDateTime.now())
                .notificationType(type)
                .status(NotificationStatus.UNREAD)
                .build();

        notificationRepository.save(notification);

        NotificationDTO notificationDTO = NotificationDTO.builder()
                .senderId(notification.getSenderId())
                .receiverId(notification.getReceiverId())
                .content(notification.getContent())
                .time(notification.getTime())
                .notificationType(notification.getNotificationType().name())
                .status(notification.getStatus().name())
                .build();

        NotificationResponseDTO responseDTO = NotificationResponseDTO.builder()
                .status("notification")
                .data(notificationDTO)
                .build();

        simpMessagingTemplate.convertAndSendToUser(String.valueOf(receiverId), "/notification", responseDTO);
    }

    @Override
    public List<NotificationEntity> getNotificationsByReceiverId(Integer receiverId) {
        return notificationRepository.findByReceiverId(receiverId);
    }

    @Scheduled(cron = "0,01 * * * * ?")
    public void sendEmailNotificationsForUpcomingSchedules() throws MessagingException {
        List<ConsultationScheduleEntity> upcomingSchedules = consultationScheduleRepository
                .findByConsultationDateAfterAndStatusConfirmedTrue(LocalDate.now());

        DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern("HH:mm");

        for (ConsultationScheduleEntity schedule : upcomingSchedules) {
            LocalTime consultationTime = LocalTime.parse(schedule.getConsultationTime(), timeFormatter);
            LocalDate consultationDate = schedule.getConsultationDate();

            LocalDateTime consultationDateTime = consultationDate.atTime(consultationTime);

            if (consultationDateTime.isAfter(LocalDateTime.now()) &&
                    consultationDateTime.isBefore(LocalDateTime.now().plusDays(1))) {
                sendReminderEmail(consultationDateTime, schedule,  24*60);
            }
            if (consultationDateTime.isAfter(LocalDateTime.now()) &&
                    consultationDateTime.isBefore(LocalDateTime.now().plusDays(1))) {
                sendReminderEmailToUser(consultationDateTime, schedule,  24*60);
            }
        }
    }

    private void sendReminderEmail(LocalDateTime consultationDateTime, ConsultationScheduleEntity schedule, int minutesBefore) throws MessagingException {
        LocalDateTime reminderTime = consultationDateTime.minusMinutes(minutesBefore);
        LocalDateTime currentTime = LocalDateTime.now();

        if (currentTime.isAfter(reminderTime.minusMinutes(1)) && currentTime.isBefore(reminderTime.plusMinutes(1))) {
            List<ConsultationScheduleRegistrationEntity> registrations = registrationRepository
                    .findByConsultationSchedule(schedule);

            for (ConsultationScheduleRegistrationEntity registration : registrations) {
                String userEmail = registration.getUser().getAccount().getEmail();
                String userFullName = registration.getUser().getLastName() + " " + registration.getUser().getFirstName();

                MimeMessage mailMessage = javaMailSender.createMimeMessage();
                MimeMessageHelper mailHelper = new MimeMessageHelper(mailMessage, true, "UTF-8");

                String emailContent = "<html><head><meta charset=\"UTF-8\"></head><body>"
                        + "<div style=\"font-family: Arial, sans-serif; padding: 20px; color: #333;\">"
                        + "    <h2 style=\"color: #0066cc; text-align: center;\">Lịch tư vấn sắp diễn ra</h2>"
                        + "    <p>Chào <strong>" + userFullName + "</strong>,</p>"
                        + "    <p>Lịch tư vấn: <strong>" + schedule.getTitle() + "</strong> sẽ diễn ra trong <strong>1 ngày nữa</strong>.</p>"
                        + "    <p>Chúng tôi rất mong được gặp bạn.</p>"
                        + "    <br>"
                        + "    <p style=\"font-size: 14px; text-align: center; color: #888;\">Trân trọng!</p>"
                        + "    <hr style=\"border: 0; border-top: 1px solid #ccc;\">"
                        + "    <p style=\"font-size: 12px; text-align: center; color: #888;\">Đây là email tự động, vui lòng không trả lời.</p>"
                        + "</div>"
                        + "</body></html>";

                mailHelper.setFrom("ngoquangnghia111003@gmail.com");
                mailHelper.setTo(userEmail);
                mailHelper.setSubject("Lịch tư vấn sắp diễn ra");
                mailHelper.setText(emailContent, true);

                emailService.sendEmail(mailMessage);
            }
        }
    }

    private void sendReminderEmailToUser(LocalDateTime consultationDateTime, ConsultationScheduleEntity schedule, int minutesBefore) throws MessagingException {
        LocalDateTime reminderTime = consultationDateTime.minusMinutes(minutesBefore);
        LocalDateTime currentTime = LocalDateTime.now();

        if (currentTime.isAfter(reminderTime.minusMinutes(1)) && currentTime.isBefore(reminderTime.plusMinutes(1))) {
            String userEmail = schedule.getUser().getAccount().getEmail();
            String userFullName = schedule.getUser().getLastName() + " " + schedule.getUser().getFirstName();

            MimeMessage mailMessage = javaMailSender.createMimeMessage();
            MimeMessageHelper mailHelper = new MimeMessageHelper(mailMessage, true, "UTF-8");

            String emailContent = "<html><head><meta charset=\"UTF-8\"></head><body>"
                    + "<div style=\"font-family: Arial, sans-serif; padding: 20px; color: #333;\">"
                    + "    <h2 style=\"color: #0066cc; text-align: center;\">Lịch tư vấn sắp diễn ra</h2>"
                    + "    <p>Chào <strong>" + userFullName + "</strong>,</p>"
                    + "    <p>Lịch tư vấn: <strong>" + schedule.getTitle() + "</strong> sẽ diễn ra trong <strong>1 ngày nữa</strong>.</p>"
                    + "    <p>Hãy chuẩn bị sẵn sàng cho buổi tư vấn.</p>"
                    + "    <br>"
                    + "    <p style=\"font-size: 14px; text-align: center; color: #888;\">Trân trọng!</p>"
                    + "    <hr style=\"border: 0; border-top: 1px solid #ccc;\">"
                    + "    <p style=\"font-size: 12px; text-align: center; color: #888;\">Đây là email tự động, vui lòng không trả lời.</p>"
                    + "</div>"
                    + "</body></html>";

            mailHelper.setFrom("ngoquangnghia111003@gmail.com");
            mailHelper.setTo(userEmail);
            mailHelper.setSubject("Lịch tư vấn sắp diễn ra");
            mailHelper.setText(emailContent, true);

            emailService.sendEmail(mailMessage);
        }
    }

//    @Override
//    public Page<studentConsulting.model.payload.dto.common.NotificationDTO> findNotificationsByUserWithFilters(Integer userId, String content, LocalDate startDate, LocalDate endDate, Pageable pageable) {
//
//        Specification<NotificationEntity> spec = Specification.where(NotificationSpecification.isReceiver(userId));
//
//        if (content != null && !content.trim().isEmpty()) {
//            spec = spec.and(NotificationSpecification.hasContent(content));
//        }
//
//        if (startDate != null && endDate != null) {
//            spec = spec.and(NotificationSpecification.hasExactDateRange(startDate, endDate));
//        } else if (startDate != null) {
//            spec = spec.and(NotificationSpecification.hasExactStartDate(startDate));
//        } else if (endDate != null) {
//            spec = spec.and(NotificationSpecification.hasDateBefore(endDate));
//        }
//
//        Page<NotificationEntity> notifications = notificationRepository.findAll(spec, pageable);
//
//        return notifications.map(this::convertToDTO);
//    }
//
//    private studentConsulting.model.payload.dto.common.NotificationDTO convertToDTO(NotificationEntity entity) {
//        UserInformationEntity sender = userRepository.findById(entity.getSenderId()).orElseThrow();
//        String email = sender.getAccount().getEmail();
//        String fullName = sender.getLastName() + sender.getFirstName();
//        return studentConsulting.model.payload.dto.common.NotificationDTO.builder()
//                .id(entity.getId())
//                .sender(studentConsulting.model.payload.dto.common.NotificationDTO.SenderDTO.builder()
//                        .id(entity.getSenderId())
//                        .email(email)
//                        .fullName(fullName)
//                        .build())
//                .content(entity.getContent())
//                .time(entity.getTime())
//                .build();
//    }
}
