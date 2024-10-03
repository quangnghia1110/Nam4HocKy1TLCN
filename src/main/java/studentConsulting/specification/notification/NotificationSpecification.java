package studentConsulting.specification.notification;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.notification.NotificationEntity;

import java.time.LocalDate;
import java.time.LocalDateTime;

public class NotificationSpecification {

    public static Specification<NotificationEntity> isReceiver(Integer userId) {
        return (root, query, criteriaBuilder) -> {
            if (userId == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.equal(root.get("receiverId"), userId);
        };
    }

    public static Specification<NotificationEntity> hasContent(String content) {
        return (root, query, criteriaBuilder) -> {
            if (content == null || content.trim().isEmpty()) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.like(criteriaBuilder.lower(root.get("content")), "%" + content.toLowerCase() + "%");
        };
    }

    public static Specification<NotificationEntity> hasExactStartDate(LocalDate startDate) {
        return (root, query, criteriaBuilder) -> {
            if (startDate == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.greaterThanOrEqualTo(root.get("time"), startDate.atStartOfDay());
        };
    }

    public static Specification<NotificationEntity> hasDateBefore(LocalDate endDate) {
        return (root, query, criteriaBuilder) -> {
            if (endDate == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.lessThanOrEqualTo(root.get("time"), endDate.atTime(23, 59, 59));
        };
    }

    public static Specification<NotificationEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (root, query, criteriaBuilder) -> {
            if (startDate == null || endDate == null) {
                return criteriaBuilder.conjunction();
            }
            LocalDateTime startDateTime = startDate.atStartOfDay();
            LocalDateTime endDateTime = endDate.atTime(23, 59, 59);
            return criteriaBuilder.between(root.get("time"), startDateTime, endDateTime);
        };
    }
}
