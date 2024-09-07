package studentConsulting.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;

@Repository
public interface ConsultationScheduleRepository extends JpaRepository<ConsultationScheduleEntity, Integer> {

	List<ConsultationScheduleEntity> findByUserId(Integer userId);

	Page<ConsultationScheduleEntity> findByUserAndDepartmentIdAndTitleContaining(UserInformationEntity user, Integer departmentId, String title, Pageable pageable);

    Page<ConsultationScheduleEntity> findByUserAndDepartmentId(UserInformationEntity user, Integer departmentId, Pageable pageable);

    Page<ConsultationScheduleEntity> findByUserAndTitleContaining(UserInformationEntity user, String title, Pageable pageable);

    Page<ConsultationScheduleEntity> findByUser(UserInformationEntity user, Pageable pageable);

}
