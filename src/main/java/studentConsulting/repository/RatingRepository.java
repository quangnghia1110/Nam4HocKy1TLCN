package studentConsulting.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.feedback.RatingEntity;

@Repository
public interface RatingRepository extends PagingAndSortingRepository<RatingEntity, Integer>, JpaSpecificationExecutor<RatingEntity> {

    @Query("SELECT r FROM RatingEntity r WHERE r.user = :user AND r.department.id = :departmentId")
    Page<RatingEntity> findByUserAndDepartment(UserInformationEntity user, Integer departmentId, Pageable pageable);

    @Query("SELECT r FROM RatingEntity r WHERE r.user = :user AND CONCAT(r.consultant.firstName, ' ', r.consultant.lastName) LIKE %:consultantName%")
    Page<RatingEntity> findByUserAndConsultantName(UserInformationEntity user, String consultantName, Pageable pageable);

    @Query("SELECT r FROM RatingEntity r WHERE r.user = :user AND r.department.id = :departmentId AND CONCAT(r.consultant.firstName, ' ', r.consultant.lastName) LIKE %:consultantName%")
    Page<RatingEntity> findByUserAndDepartmentAndConsultantName(UserInformationEntity user, Integer departmentId, String consultantName, Pageable pageable);

    @Query("SELECT r FROM RatingEntity r WHERE r.user = :user")
    Page<RatingEntity> findByUser(UserInformationEntity user, Pageable pageable);
}

