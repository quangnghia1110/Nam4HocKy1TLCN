package studentConsulting.repository.consultation_schedule;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleEntity;
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleRegistrationEntity;
import studentConsulting.model.entity.user.UserInformationEntity;

import java.util.List;
import java.util.Optional;

@Repository
public interface ConsultationScheduleRepository extends PagingAndSortingRepository<ConsultationScheduleEntity, Integer>, JpaSpecificationExecutor<ConsultationScheduleEntity> {

    List<ConsultationScheduleEntity> findByUserId(Integer userId);

    Page<ConsultationScheduleEntity> findByUserAndDepartmentIdAndTitleContaining(UserInformationEntity user,
                                                                                 Integer departmentId, String title, Pageable pageable);

    Page<ConsultationScheduleEntity> findByUserAndDepartmentId(UserInformationEntity user, Integer departmentId,
                                                               Pageable pageable);

    Page<ConsultationScheduleEntity> findByUserAndTitleContaining(UserInformationEntity user, String title,
                                                                  Pageable pageable);

    Page<ConsultationScheduleEntity> findByUser(UserInformationEntity user, Pageable pageable);

    @Query("SELECT c FROM ConsultationScheduleEntity c WHERE c.id = :scheduleId")
    Optional<ConsultationScheduleEntity> findConsulationScheduleById(@Param("scheduleId") Integer scheduleId);

    @Query("SELECT c FROM ConsultationScheduleRegistrationEntity c WHERE c.consultationSchedule.id = :scheduleId")
    Optional<ConsultationScheduleRegistrationEntity> findConsultationScheduleByScheduleId(@Param("scheduleId") Integer scheduleId);

    boolean existsByIdAndCreatedBy(Integer id, Integer createdBy);

    @Query("SELECT c FROM ConsultationScheduleEntity c WHERE c.id = :scheduleId AND c.consultant.account.department.id = :departmentId")
    Optional<ConsultationScheduleEntity> findByIdAndDepartmentId(@Param("scheduleId") Integer scheduleId, @Param("departmentId") Integer departmentId);

    @Query("SELECT c FROM ConsultationScheduleEntity c WHERE c.id = :scheduleId AND c.createdBy.id = :createdById")
    Optional<ConsultationScheduleEntity> findByIdAndCreatedBy(@Param("scheduleId") Integer scheduleId, @Param("createdById") Integer createdById);

}
