package studentConsulting.repository.authentication;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.entity.authentication.AccountEntity;
import studentConsulting.entity.authentication.UserEntity;

import java.util.Optional;

@Repository
public interface UserRepository extends JpaRepository<UserEntity, Long> {

    @Query("SELECT u FROM UserEntity u WHERE u.accountModel=:account")
    UserEntity findUserInfoModelByAccountModel(@Param("account") AccountEntity accountModel);

    @Query("SELECT u FROM UserEntity u WHERE u.id=:id")
    Optional<UserEntity> findById(@Param("id") Long id);

    @Query("SELECT u FROM UserEntity u WHERE u.accountModel.roleModel.name = :rolename")
    Iterable<UserEntity> findAllByRoleName(@Param("rolename") String rolename);
}
