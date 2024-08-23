package studentConsulting.repository.authentication;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;

import java.util.Optional;

@Repository
public interface UserRepository extends JpaRepository<UserInformationEntity, Long> {

    @Query("SELECT u FROM UserInformationEntity u WHERE u.account=:account")
    UserInformationEntity findUserInfoModelByAccountModel(@Param("account") AccountEntity accountModel);

    @Query("SELECT u FROM UserInformationEntity u WHERE u.id=:id")
    Optional<UserInformationEntity> findById(@Param("id") Integer integer);

    @Query("SELECT u FROM UserInformationEntity u WHERE u.account.role.name = :rolename")
    Iterable<UserInformationEntity> findAllByRoleName(@Param("rolename") String rolename);
    
	boolean existsByPhone(String phone);

}
