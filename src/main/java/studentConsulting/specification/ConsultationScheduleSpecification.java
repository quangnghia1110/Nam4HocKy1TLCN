package studentConsulting.specification;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;

public class ConsultationScheduleSpecification {

    // Lọc theo tư vấn viên
    public static Specification<ConsultationScheduleEntity> hasConsultant(UserInformationEntity consultant) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get("consultant"), consultant);
    }

    // Lọc theo tiêu đề
    public static Specification<ConsultationScheduleEntity> hasTitle(String title) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.like(root.get("title"), "%" + title + "%");
    }

    // Lọc theo trạng thái công khai
    public static Specification<ConsultationScheduleEntity> hasStatusPublic(Boolean statusPublic) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get("statusPublic"), statusPublic);
    }

    // Lọc theo trạng thái xác nhận
    public static Specification<ConsultationScheduleEntity> hasStatusConfirmed(Boolean statusConfirmed) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get("statusConfirmed"), statusConfirmed);
    }

    // Lọc theo mode (online/offline)
    public static Specification<ConsultationScheduleEntity> hasMode(Boolean mode) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get("mode"), mode);
    }
}
