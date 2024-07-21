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

    @Query("SELECT u FROM UserEntity u WHERE u.accountModel=:account")
    UserInformationEntity findUserInfoModelByAccountModel(@Param("account") AccountEntity accountModel);

    @Query("SELECT u FROM UserEntity u WHERE u.id=:id")
    Optional<UserInformationEntity> findById(@Param("id") Long id);

    @Query("SELECT u FROM UserEntity u WHERE u.accountModel.roleModel.name = :rolename")
    Iterable<UserInformationEntity> findAllByRoleName(@Param("rolename") String rolename);
}
