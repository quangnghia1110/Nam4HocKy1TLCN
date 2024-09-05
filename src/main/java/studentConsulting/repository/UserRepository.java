package studentConsulting.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;

@Repository
public interface UserRepository extends JpaRepository<UserInformationEntity, Long> {

    @Query("SELECT u FROM UserInformationEntity u WHERE u.account=:account")
    UserInformationEntity findUserInfoModelByAccountModel(@Param("account") AccountEntity accountModel);

    @Query("SELECT u FROM UserInformationEntity u WHERE u.id=:id")
    Optional<UserInformationEntity> findById(@Param("id") Integer integer);

    @Query("SELECT u FROM UserInformationEntity u WHERE u.account.role.name = :rolename")
    Iterable<UserInformationEntity> findAllByRoleName(@Param("rolename") String rolename);
    
	boolean existsByPhone(String phone);

    boolean existsByStudentCode(String studentCode);

    boolean existsByAccount_Email(String email);

    Optional<UserInformationEntity> findByAccount_Username(String username);

    Optional<UserInformationEntity> findByStudentCode(String studentCode);
    
    //Iterator hỗ trợ forEach, không tải lên toàn bộ và k yêu câu lưu trữ
    @Query("SELECT u FROM UserInformationEntity u WHERE u.account.role.name = :rolename AND u.account.department.id = :departmentId")
    Iterable<UserInformationEntity> findAllByRoleNameAndDepartment(@Param("rolename") String rolename, @Param("departmentId") Integer departmentId);

    @Query("SELECT u FROM UserInformationEntity u WHERE LOWER(u.firstName) LIKE LOWER(CONCAT('%', :name, '%'))")
    List<UserInformationEntity> findByName(@Param("name") String name);
}
