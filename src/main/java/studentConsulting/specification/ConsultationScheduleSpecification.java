package studentConsulting.specification;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;

public class ConsultationScheduleSpecification {

	public static Specification<ConsultationScheduleEntity> hasDepartment(Integer departmentId) {
		return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("department").get("id"), departmentId);
	}

	public static Specification<ConsultationScheduleEntity> hasUser(UserInformationEntity user) {
		return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("user"), user);
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
}
