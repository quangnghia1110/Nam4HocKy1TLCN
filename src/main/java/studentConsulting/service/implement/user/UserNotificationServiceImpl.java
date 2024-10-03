package studentConsulting.service.implement.user;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.notification.NotificationDTO;
import studentConsulting.repository.notification.NotificationRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.user.IUserNotificationService;
import studentConsulting.specification.notification.NotificationSpecification;

import java.time.LocalDate;

@Service
public class UserNotificationServiceImpl implements IUserNotificationService {
    @Autowired
    private NotificationRepository notificationRepository;

    @Autowired
    private UserRepository userRepository;

    @Override
    public Page<NotificationDTO> findNotificationsByUserWithFilters(Integer userId, String content, LocalDate startDate, LocalDate endDate, Pageable pageable) {

        Specification<NotificationEntity> spec = Specification.where(NotificationSpecification.isReceiver(userId));

        if (content != null && !content.trim().isEmpty()) {
            spec = spec.and(NotificationSpecification.hasContent(content));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(NotificationSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(NotificationSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(NotificationSpecification.hasDateBefore(endDate));
        }

        Page<NotificationEntity> notifications = notificationRepository.findAll(spec, pageable);

        return notifications.map(this::convertToDTO);
    }

    private NotificationDTO convertToDTO(NotificationEntity entity) {
        UserInformationEntity sender = userRepository.findById(entity.getSenderId()).orElseThrow();
        String email = sender.getAccount().getEmail();
        String fullName = sender.getLastName() + sender.getFirstName();
        return NotificationDTO.builder()
                .id(entity.getId())
                .sender(NotificationDTO.SenderDTO.builder()
                        .id(entity.getSenderId())
                        .email(email)
                        .fullName(fullName)
                        .build())
                .content(entity.getContent())
                .time(entity.getTime())
                .build();
    }


}
