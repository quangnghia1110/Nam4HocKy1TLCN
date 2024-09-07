package studentConsulting.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.feedback.RatingEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;

@Repository
public interface RatingRepository extends JpaRepository<RatingEntity, Long> {

    // Tìm kiếm đánh giá theo người dùng và phòng ban
    @Query("SELECT r FROM RatingEntity r WHERE r.user = :user AND r.department.id = :departmentId")
    Page<RatingEntity> findByUserAndDepartment(UserInformationEntity user, Integer departmentId, Pageable pageable);

    // Tìm kiếm đánh giá theo người dùng và tên tư vấn viên 
    @Query("SELECT r FROM RatingEntity r WHERE r.user = :user AND CONCAT(r.consultant.firstName, ' ', r.consultant.lastName) LIKE %:consultantName%")
    Page<RatingEntity> findByUserAndConsultantName(UserInformationEntity user, String consultantName, Pageable pageable);

    // Tìm kiếm đánh giá theo người dùng, phòng ban và tên tư vấn viên
    @Query("SELECT r FROM RatingEntity r WHERE r.user = :user AND r.department.id = :departmentId AND CONCAT(r.consultant.firstName, ' ', r.consultant.lastName) LIKE %:consultantName%")
    Page<RatingEntity> findByUserAndDepartmentAndConsultantName(UserInformationEntity user, Integer departmentId, String consultantName, Pageable pageable);
    // Tìm tất cả đánh giá của người dùng
    @Query("SELECT r FROM RatingEntity r WHERE r.user = :user")
    Page<RatingEntity> findByUser(UserInformationEntity user, Pageable pageable);
}

