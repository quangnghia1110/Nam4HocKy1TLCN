package studentConsulting.repository.authentication;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.entity.authentication.roleAuthEntity;

@Repository
public interface roleAuthRepository extends JpaRepository<roleAuthEntity, Long> {

    @Query("SELECT t FROM roleAuthEntity t WHERE t.userModel.id = :userId")
    roleAuthEntity findByUserId(@Param("userId") Long userId);

    @Query("SELECT t FROM roleAuthEntity  t WHERE t.tokenId = :tokenId")
    roleAuthEntity findByTokenId(@Param("tokenId") String tokenId);

    @Modifying
    @Query("DELETE FROM roleAuthEntity  t WHERE t.tokenId = :tokenId")
    int deleteTokenByTokenId(@Param("tokenId") String tokenId);
}
