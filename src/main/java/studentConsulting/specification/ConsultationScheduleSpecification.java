package studentConsulting.specification;

import java.time.LocalDate;

import org.springframework.data.jpa.domain.Specification;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;

public class ConsultationScheduleSpecification {

	public static Specification<ConsultationScheduleEntity> hasDepartment(Integer departmentId) {
		return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("department").get("id"), departmentId);
	}

	public static Specification<ConsultationScheduleEntity> hasUser(UserInformationEntity user) {
		return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("user"), user);
	}
	
	public static Specification<ConsultationScheduleEntity> hasUser(Integer userId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("user").get("id"), userId);
    }

	public static Specification<ConsultationScheduleEntity> hasConsultant(Integer consultantId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("consultant").get("id"), consultantId);
    }
	
	public static Specification<ConsultationScheduleEntity> hasConsultant(UserInformationEntity consultant) {
		return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("consultant"), consultant);
	}

	public static Specification<ConsultationScheduleEntity> hasTitle(String title) {
		return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("title"), "%" + title + "%");
	}

	public static Specification<ConsultationScheduleEntity> hasStatusPublic(Boolean statusPublic) {
		return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("statusPublic"), statusPublic);
	}

	public static Specification<ConsultationScheduleEntity> hasStatusConfirmed(Boolean statusConfirmed) {
		return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("statusConfirmed"), statusConfirmed);
	}

	public static Specification<ConsultationScheduleEntity> hasMode(Boolean mode) {
		return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("mode"), mode);
	}
	public static Specification<ConsultationScheduleEntity> hasExactStartDate(LocalDate startDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("createdAt").as(LocalDate.class), startDate);
    }

	public static Specification<ConsultationScheduleEntity> hasDateBefore(LocalDate endDate) {
	    return (root, query, cb) -> cb.lessThanOrEqualTo(root.get("createdAt").as(LocalDate.class), endDate);
	}

    public static Specification<ConsultationScheduleEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.between(root.get("createdAt").as(LocalDate.class), startDate, endDate);
    }
    
}
