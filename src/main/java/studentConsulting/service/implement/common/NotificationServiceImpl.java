package studentConsulting.service.implement.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.NotificationEntity;
import studentConsulting.model.payload.dto.common.NotificationResponseDTO;
import studentConsulting.model.payload.dto.common.NotificationResponseDTO.NotificationDTO;
import studentConsulting.repository.common.NotificationRepository;
import studentConsulting.service.interfaces.common.INotificationService;

import java.time.LocalDateTime;
import java.util.List;

@Service
public class NotificationServiceImpl implements INotificationService {

    private final NotificationRepository notificationRepository;
    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

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
