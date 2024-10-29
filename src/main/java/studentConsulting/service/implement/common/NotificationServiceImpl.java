package studentConsulting.service.implement.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.payload.dto.notification.NotificationResponseDTO.NotificationDTO;
import studentConsulting.repository.notification.NotificationRepository;
import studentConsulting.service.interfaces.common.INotificationService;

import java.util.List;

@Service
public class NotificationServiceImpl implements INotificationService {

    private final NotificationRepository notificationRepository;

    @Autowired
    public NotificationServiceImpl(NotificationRepository notificationRepository) {
        this.notificationRepository = notificationRepository;
    }

    @Override
    public void sendNotification(NotificationDTO notificationDTO) {
        NotificationEntity notificationEntity = NotificationEntity.builder()
                .senderId(notificationDTO.getSenderId())
                .receiverId(notificationDTO.getReceiverId())
                .content(notificationDTO.getContent())
                .time(notificationDTO.getTime())
                .notificationType(NotificationType.valueOf(notificationDTO.getNotificationType())) // Convert from string to enum
                .status(NotificationStatus.valueOf(notificationDTO.getStatus())) // Convert from string to enum
                .build();

        notificationRepository.save(notificationEntity);
    }

    @Override
    public List<NotificationEntity> getNotificationsByReceiverId(Integer receiverId) {
        return notificationRepository.findByReceiverId(receiverId);
    }

//    @Override
//    public Page<studentConsulting.model.payload.dto.notification.NotificationDTO> findNotificationsByUserWithFilters(Integer userId, String content, LocalDate startDate, LocalDate endDate, Pageable pageable) {
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
//    private studentConsulting.model.payload.dto.notification.NotificationDTO convertToDTO(NotificationEntity entity) {
//        UserInformationEntity sender = userRepository.findById(entity.getSenderId()).orElseThrow();
//        String email = sender.getAccount().getEmail();
//        String fullName = sender.getLastName() + sender.getFirstName();
//        return studentConsulting.model.payload.dto.notification.NotificationDTO.builder()
//                .id(entity.getId())
//                .sender(studentConsulting.model.payload.dto.notification.NotificationDTO.SenderDTO.builder()
//                        .id(entity.getSenderId())
//                        .email(email)
//                        .fullName(fullName)
//                        .build())
//                .content(entity.getContent())
//                .time(entity.getTime())
//                .build();
//    }
}
