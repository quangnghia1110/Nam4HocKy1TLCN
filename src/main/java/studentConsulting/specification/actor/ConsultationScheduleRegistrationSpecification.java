package studentConsulting.specification.actor;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.ConsultationScheduleEntity;
import studentConsulting.model.entity.ConsultationScheduleRegistrationEntity;
import studentConsulting.model.entity.UserInformationEntity;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import java.time.LocalDate;

public class ConsultationScheduleRegistrationSpecification {

    public static Specification<ConsultationScheduleRegistrationEntity> hasUser(UserInformationEntity user) {
        return (root, query, cb) -> cb.equal(root.get("user"), user);
    }

    public static Specification<ConsultationScheduleRegistrationEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (root, query, cb) -> cb.between(root.get("registeredAt"), startDate, endDate);
    }

    public static Specification<ConsultationScheduleRegistrationEntity> hasStartDateAfterOrEqual(LocalDate startDate) {
        return (root, query, cb) -> cb.greaterThanOrEqualTo(root.get("registeredAt"), startDate);
    }

    public static Specification<ConsultationScheduleRegistrationEntity> hasEndDateBeforeOrEqual(LocalDate endDate) {
        return (root, query, cb) -> cb.lessThanOrEqualTo(root.get("registeredAt"), endDate);
    }

    public static Specification<ConsultationScheduleRegistrationEntity> hasConsultationSchedule(Integer consultationScheduleId) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get("consultationSchedule").get("id"), consultationScheduleId);
    }

    public static Specification<ConsultationScheduleRegistrationEntity> hasStatus(Boolean status) {
        return (root, query, cb) -> cb.equal(root.get("status"), status);
    }

    public static Specification<ConsultationScheduleRegistrationEntity> hasCreatedBy(Integer userId) {
        return (root, query, cb) -> {
            Join<ConsultationScheduleRegistrationEntity, ConsultationScheduleEntity> scheduleJoin = root.join("consultationSchedule", JoinType.INNER);
            return cb.equal(scheduleJoin.get("createdBy"), userId);
        };
    }

}
